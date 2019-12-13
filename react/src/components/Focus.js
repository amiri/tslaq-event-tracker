import React, { useRef, useEffect, useState } from 'react';
import moment from 'moment';
require('moment-timezone');
import * as d3 from 'd3';
import {schemeBlues,schemeSpectral,schemePaired} from 'd3-scale-chromatic';
import {
  getXScale,
  getYScale,
  getLines,
  getTickVals,
  getZoom,
  updateClipPath,
  updateXAxis,
  updateYAxis,
  updateZeroLine,
  updateHighLine,
  updateLowLine,
} from './utils/Chart';
import {
  Annotation,
  AnnotationCallout,
  AnnotationXYThreshold,
  AnnotationCalloutCircle,
} from 'react-annotation';
import { flatten, isEmpty, sortedUniq } from 'lodash';
import {Typography} from 'antd';

const {Text} = Typography;
const Focus = ({
  width,
  height,
  margin,
  ps,
  config,
  zoomF,
  zoomDomain,
  events,
  resolution,
}) => {
  // Extents
  const xExtent = d3.extent(ps, p => p.priceTime);
  const yExtent = d3.extent(ps, p => p.close);

  // Scales
  const xScale = getXScale({ xExtent, width, margin });
  const yScale = getYScale({ yExtent, height, margin });
  const categories = sortedUniq(flatten(events.map(e => e.categories.map(c => c.name))).sort());
  const colors = d3.scaleOrdinal().range(d3.schemeSpectral[categories.length < 10 ? 10 : categories.length]).domain(categories);
  const [hover, setHover] = useState(null);

  // Annotations
  const rawAnnotations = events.map(e => {
    const interval = resolution === 'daily' ? 'day' : 'hour';
    const fmt = resolution === 'daily' ? 'YYYY-MM-DD' : 'YYYY-MM-DD HH:MM:SS';
    const priceMatch = ps.filter(p =>
      moment(p.priceTime).isBetween(
        e.eventTime.clone().subtract(1, interval),
        e.eventTime.clone().add(1, interval),
      ),
    );
    const p = !isEmpty(priceMatch) ? priceMatch[0].close : 0;
    const title = `${e.eventTime.tz('America/New_York').format('ddd, MMM DD YYYY, h:mm:ss a z')}: ${e.title}`;
    return {
      note: { title, label: e.body},
      x: xScale(e.eventTime.toDate()),
      y: yScale(p),
      category: e.categories[0].name,
    };
  });
  const annotations = rawAnnotations.map((a, i) => {
    const { note, x, y, category } = a;
    const radius = 5;
    note.wrap = 200;
    note.orientation = null;
    note.align = 'dynamic';
    note.lineType = null;
    note.bgPadding = 10;
    const thisY =
      rawAnnotations[i - 1] && y === rawAnnotations[i - 1].y
        ? y + radius + 5
        : y;
    return (
      <g key={i}>
        <AnnotationCallout
          x={x}
          y={thisY}
          dx={20}
          color={'#444444'}
          dy={20}
          note={note}
          className={hover === i ? '' : 'hidden'}
        />
        <circle
          fill={colors(category)}
          r={radius}
          cx={x}
          cy={thisY}
          onMouseOver={() => setHover(i)}
          onMouseOut={() => setHover(null)}
          onClick={() => handleClick(i)}
        />
      </g>
    );
  });

  // Zoom
  function zoomed() {
    if (!d3.event.sourceEvent) {
      // console.log('No zoom sourceEvent');
      // console.log('no zoom sourceEvent arguments: ', arguments);
      // zoomF({params: xScale.range(), eventType: null});
      // return;
      // return;
    }
    if (d3.event.sourceEvent && d3.event.sourceEvent.type === 'brush') return; // ignore zoom-by-brush
    if (d3.event.sourceEvent) {
      // console.log('zoom sourceEvent.type: ', d3.event.sourceEvent.type);
    }
    const t = d3.event.transform;
    const move = xScale.range().map(t.invertX, t);
    // console.log('params in Focus: ', move);
    zoomF({
      params: move,
      eventType: d3.event.sourceEvent ? d3.event.sourceEvent.type : null,
    });
  }

  const zoom = getZoom({ width, height, zoomed });

  const focusRef = useRef(null);
  const svgRef = useRef(null);
  const { timeZone } = config;

  // Prices
  useEffect(() => {
    const svg = d3.select(svgRef.current);
    const focus = d3.select(focusRef.current);
    const { tickVals, tickFmt } = getTickVals({ xExtent, timeZone });

    // ClipPath
    const clipPath = svg.selectAll('defs').data([0]);
    updateClipPath({ s: clipPath, width, height, margin });

    // FocusYAxis
    const focusYAxis = focus.selectAll('.y-axis').data([0]);
    updateYAxis({ s: focusYAxis, margin, yScale, width });

    // FocusXAxis
    const focusXAxis = focus.selectAll('.x-axis').data([0]);
    updateXAxis({
      s: focusXAxis,
      xScale,
      tickVals,
      tickFmt,
      height,
      margin,
    });

    const zeroLine = focus.selectAll('.zero').data([0]);
    updateZeroLine({
      s: zeroLine,
      yScale,
      yExtent,
      width,
      margin,
    });

    const highLine = focus.selectAll('.high').data([0]);
    updateHighLine({
      s: highLine,
      margin,
      width,
      yScale,
      yExtent,
    });

    const lowLine = focus.selectAll('.low').data([0]);
    updateLowLine({
      s: lowLine,
      margin,
      width,
      yScale,
      yExtent,
    });

    const focusZoom = svg.selectAll('.zoom');
    focusZoom.call(zoom);
  }, [svgRef, focusRef, height, width, yExtent, xExtent]);

  // Zoom
  useEffect(() => {
    if (zoomDomain[0] >= 0 && zoomDomain[1] >= 0) {
      const svg = d3.select(svgRef.current);
      const focusZoom = svg.selectAll('.zoom');
      // console.log('zoomDomain in focus: ', zoomDomain);
      // console.log('width in focus: ', width);
      focusZoom.call(
        zoom.transform,
        d3.zoomIdentity
          .scale(
            (width - margin.left - margin.right) /
              (zoomDomain[1] - zoomDomain[0]),
          )
          .translate(-zoomDomain[0] + margin.left, 0),
        // .translate(-zoomDomain[0], 0),
      );
    }
  }, [zoomDomain]);

  return (
    <svg
      ref={svgRef}
      className='focus-svg'
      preserveAspectRatio='xMinYMin meet'
      viewBox={`0 0 ${width ? width : 0} ${height ? height + 5 : 0}`}
      transform={`translate(0, ${margin.top})`}
    >
      <g className='focus' ref={focusRef}>
        <path className='line' d={getLines({ xScale, yScale })(ps)} />
      </g>
      <rect
        className='zoom'
        width={width ? width - margin.left - margin.right : 0}
        height={height ? height : 0}
        transform={`translate(${margin.left},${margin.top})`}
      />
      <g className='annotation-group'>{annotations}</g>
    </svg>
  );
};

export default Focus;
