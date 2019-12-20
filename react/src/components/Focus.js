import React, { useRef, useEffect, useState, useMemo } from 'react';
import { useHistory } from 'react-router-dom';
import moment from 'moment';
require('moment-timezone');
import * as d3 from 'd3';
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
  openViewModal,
  openNewEventModal,
  encryptIds,
} from './utils/Chart';
import { AnnotationCallout } from 'react-annotation';
import { isEmpty, compact } from 'lodash';

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
  const history = useHistory();
  d3.select('.focus-svg')
    .on('touchstart', noZoom)
    .on('touchmove', noZoom);
  // History
  // const [selectedEvents, setSelectedEvents] = useState({});

  // Extents
  const xExtent = d3.extent(ps, p => p.priceTime);
  const yExtent = d3.extent(ps, p => p.close);

  // Scales
  const xScale = getXScale({ xExtent, width, margin });
  const yScale = getYScale({ yExtent, height, margin });

  const colors = d3
    .scaleOrdinal()
    .range(
      d3.schemeSpectral[
        config.categories.length < 10 ? 10 : config.categories.length
      ],
    )
    .domain(config.categories);

  const [hover, setHover] = useState(null);

  function clickZoom(d) {
    console.log(d);
    console.log(d.pageX);
    console.log(d.pageY);
    if (d.defaultPrevented) return; // zoomed

    openNewEventModal({ eventDate: xScale.invert(d.pageX) });
  }
  function noZoom() {
    d3.event.preventDefault();
  }

  // Annotations
  const rawAnnotations = useMemo(
    () =>
      events
        ? events.map(e => {
            const interval = resolution === 'daily' ? 'day' : 'hour';
            const priceMatch = ps.filter(p =>
              moment(p.priceTime).isBetween(
                e.eventTime.clone().subtract(1, interval),
                e.eventTime.clone().add(1, interval),
              ),
            );
            const p = !isEmpty(priceMatch) ? priceMatch[0].close : 0;
            const title = `${e.eventTime
              .tz('America/New_York')
              .format('ddd, MMM DD YYYY, h:mm:ss a z')}: ${e.title}`;
            return {
              note: { title, label: e.body },
              x: xScale(e.eventTime.toDate()),
              y: yScale(p),
              categories: e.categories,
              id: e.id,
            };
          })
        : null,
    [events],
  );

  const annotations = rawAnnotations
    ? rawAnnotations.map((a, i) => {
        const { note, x, y, categories } = a;
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
        const circleClasses = compact([
          'note-circle',
          ...categories.map(c => c.id),
          // selectedEvents[a.id] === true ? 'selected' : null,
        ]);
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
              fill={colors(categories[0].name)}
              r={radius}
              cx={x}
              cy={thisY}
              id={a.id}
              className={circleClasses.join(' ')}
              onMouseOver={() => setHover(i)}
              onFocus={() => setHover(i)}
              onMouseOut={() => setHover(null)}
              onBlur={() => setHover(null)}
              onClick={() =>
                openViewModal({ id: encryptIds({ ids: [a.id] }), history })
              }
            />
          </g>
        );
      })
    : null;

  // Zoom
  function zoomed() {
    if (d3.event.sourceEvent && d3.event.sourceEvent.type === 'brush') return; // ignore zoom-by-brush
    const t = d3.event.transform;
    const move = xScale.range().map(t.invertX, t);
    zoomF({
      params: move,
      eventType: d3.event.sourceEvent ? d3.event.sourceEvent.type : null,
    });
  }

  const zoom = getZoom({ width, height, zoomed });

  const focusRef = useRef(null);
  const svgRef = useRef(null);
  const { timeZone } = config;

  // Select circles
  // function selectCircles() {
  //   const extent = d3.event.selection;
  //   const selections = {};
  //   annotations.map(a => {
  //     const { cx, cy, id } = a.props.children[1].props;
  //     const selected = isSelected({ coords: extent, cx, cy });
  //     selections[id] = selected;
  //   });
  //   setSelectedEvents(selections);
  // }

  // Select Circle Brush
  // const brush = d3
  //   .brush()
  //   .extent([
  //     [0, 0],
  //     [width, height],
  //   ])
  //   .on('brush', selectCircles);

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

    // svg.call(brush);
  }, [svgRef, focusRef, height, width, yExtent, xExtent]);

  // Zoom
  useEffect(() => {
    if (zoomDomain[0] >= 0 && zoomDomain[1] >= 0) {
      const svg = d3.select(svgRef.current);
      const focusZoom = svg.selectAll('.zoom');
      focusZoom.call(
        zoom.transform,
        d3.zoomIdentity
          .scale(
            (width - margin.left - margin.right) /
              (zoomDomain[1] - zoomDomain[0]),
          )
          .translate(-zoomDomain[0] + margin.left, 0),
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
        onClick={clickZoom}
      />
      <g className='annotation-group'>{annotations}</g>
    </svg>
  );
};

export default Focus;
