import React, { useRef, useEffect, useState, useMemo } from 'react';
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
  margin,
  synopsis,
} from './utils/Chart';
import { AnnotationCalloutCircle } from 'react-annotation';
import { isNil, isEmpty, compact } from 'lodash';
import ReactGA from 'react-ga';

const Focus = ({
  width,
  height,
  ps,
  config,
  zoomF,
  zoomDomain,
  events,
  resolution,
  history,
  location,
  colorScale,
}) => {
  d3.select('.focus-svg')
    .on('touchstart', noZoom)
    .on('touchmove', noZoom);

  // console.log(events);
  // Extents
  const xExtent = d3.extent(ps, p => p.priceTime);
  // const xExtent1 = d3.extent(events, e => e.time).map(x => moment(x));
  // const xExtent2 = [
  //   min([xExtent[0], xExtent1[0]]),
  //   max([xExtent[1], xExtent1[1]]),
  // ];
  // console.log('xExtent: ', xExtent);
  // console.log('xExtent1: ', xExtent1);
  // console.log('xExtent2: ', xExtent2);
  const yExtent = d3.extent(ps, p => p.close);

  // Scales
  const xScale = getXScale({ xExtent, width });
  const yScale = getYScale({ yExtent, height });

  const [xStart, xEnd] = xScale.range();
  const xMidPoint = (xEnd - xStart) / 2;

  const [yStart, yEnd] = yScale.range();
  const yMidPoint = (yEnd - yStart) / 2;

  const [hover, setHover] = useState(null);

  function clickZoom(d) {
    if (d.defaultPrevented) return; // zoomed

    const eventDate = xScale.invert(d.pageX);
    ReactGA.event({
      category: 'Modal',
      action: 'OpenNewEvent',
      transport: 'beacon',
    });

    openNewEventModal({ eventDate, history, location });
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
                e.eventTime.clone().subtract(2, interval),
                e.eventTime.clone().add(2, interval),
              ),
            );
            const p = !isEmpty(priceMatch) ? priceMatch[0].close : 10;
            const title = `${e.eventTime
              .tz('America/New_York')
              .format('dddd, MMM DD, YYYY, h:mm:ss A z')}: ${e.title}`;
            const body = synopsis(JSON.parse(e.body)[0].children[0].text);
            return {
              note: { title, label: body },
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
        const cs = isNil(categories) ? [] : categories;
        const radius = 5;
        note.wrap = 200;
        note.orientation = 'leftRight';
        note.align = 'top';
        note.lineType = null;
        note.bgPadding = 10;
        const thisY =
          rawAnnotations[i - 1] && y === rawAnnotations[i - 1].y
            ? y + radius + 5
            : y;
        const circleClasses = compact(['note-circle', ...cs.map(c => c.id)]);
        return (
          <g key={i}>
            <AnnotationCalloutCircle
              x={x}
              y={thisY}
              dx={x > xMidPoint ? -20 : 20}
              color={'#444444'}
              dy={y > yMidPoint ? -20 : 20}
              note={note}
              className={hover === i ? '' : 'hidden'}
              subject={{ radius }}
            />
            <circle
              fill={isEmpty(cs) ? 'black' : colorScale(cs[0].fullName)}
              r={radius}
              cx={x}
              cy={thisY}
              id={a.id}
              className={circleClasses.join(' ')}
              onMouseOver={() => setHover(i)}
              onFocus={() => setHover(i)}
              onMouseOut={() => setHover(null)}
              onBlur={() => setHover(null)}
              onClick={() => {
                ReactGA.event({
                  category: 'Modal',
                  action: 'OpenEventsDetail',
                  transport: 'beacon',
                });
                openViewModal({ id: encryptIds({ ids: [a.id] }), history });
              }}
            />
          </g>
        );
      })
    : null;

  // Zoom
  function zoomed() {
    if (d3.event.sourceEvent && d3.event.sourceEvent.type === 'brush') return; // ignore zoom-by-brush
    // const zoomType = d3.event && d3.event.sourceEvent && d3.event.sourceEvent.type ? d3.event.sourceEvent.type : '';
    const t = d3.event.transform;
    const move = xScale.range().map(t.invertX, t);

    const xRange = xScale.range();
    const sel = [
      move[0] < xRange[0] ? xRange[0] : move[0],
      move[1] > xRange[1] ? xRange[1] : move[1],
    ];
    // console.log('Focus zoom zoomType: ', zoomType);
    // console.log('Focus zoom move: ', sel);
    zoomF({
      params: sel,
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
    updateClipPath({ s: clipPath, width, height });

    // FocusYAxis
    const focusYAxis = focus.selectAll('.y-axis').data([0]);
    updateYAxis({ s: focusYAxis, yScale, width });

    // FocusXAxis
    const focusXAxis = focus.selectAll('.x-axis').data([0]);
    updateXAxis({
      s: focusXAxis,
      xScale,
      tickVals,
      tickFmt,
      height,
    });

    const zeroLine = focus.selectAll('.zero').data([0]);
    updateZeroLine({
      s: zeroLine,
      yScale,
      yExtent,
      width,
    });

    const highLine = focus.selectAll('.high').data([0]);
    updateHighLine({
      s: highLine,
      width,
      yScale,
      yExtent,
    });

    const lowLine = focus.selectAll('.low').data([0]);
    updateLowLine({
      s: lowLine,
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
      // console.log('Focus scaling to: ', zoomDomain);
      const focus = d3.select(svgRef.current);
      const focusZoom = focus.selectAll('.zoom');
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
