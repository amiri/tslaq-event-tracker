/* eslint-disable no-unused-vars, no-inner-declarations */
import React, { useContext, useEffect, useState, useRef } from 'react';
import { EventsContext } from '../contexts/EventsContext';
import { PricesContext } from '../contexts/PricesContext';
import { ChartContext } from '../contexts/ChartContext';
import moment from 'moment';
import * as d3 from 'd3';
import { isEmpty } from 'lodash';
import { Spin } from 'antd';
require('moment-timezone');
import useComponentSize from '@rehooks/component-size';
import {
  calculateDimensions,
  getBrush,
  getZoom,
  getTickVals,
  getXAxis,
  getXScale,
  getYScale,
  updateClipPath,
  updateContextBrush,
  updateContextLines,
  updateContextXAxis,
  updateFocusLines,
  updateFocusXAxis,
  updateFocusYAxis,
  updateZoom,
  draw,
} from './utils/Chart';

const Chart = () => {
  const { events } = useContext(EventsContext);
  const { prices } = useContext(PricesContext);
  const { config, setConfig } = useContext(ChartContext);
  const [spin, setSpin] = useState(true);

  const chartRef = useRef(null);
  const svgRef = useRef(null);
  const contextRef = useRef(null);
  const focusRef = useRef(null);

  const dimensions = useComponentSize(chartRef);
  const { height, width } = dimensions;

  useEffect(() => {
    if (
      height &&
      width &&
      config &&
      !isEmpty(prices) &&
      !isEmpty(events) &&
      svgRef.current &&
      focusRef.current &&
      contextRef.current
    ) {
      const { timeZone, resolution, dateRange } = config;
      const { margin, heightContext, heightFocus } = calculateDimensions({
        height,
        width,
      });

      const priceList = resolution === 'daily' ? prices.daily : prices.hourly;
      const timeField = resolution === 'daily' ? 'partialTime' : 'priceTime';

      const ps = priceList
        .map(p => {
          const m =
            resolution === 'daily'
              ? moment(p[timeField])
                  .tz('America/New_York')
                  .tz(timeZone)
              : moment.utc(p[timeField]).tz(timeZone);
          return Object.assign(p, { priceTime: m });
        })
        .filter(p =>
          isEmpty(dateRange)
            ? true
            : p.priceTime.isSameOrAfter(dateRange[0]) &&
              p.priceTime.isSameOrBefore(dateRange[1]),
        );

      if (!isEmpty(ps)) {
        draw({
          ps,
          events,
          width,
          height,
          margin,
          heightContext,
          heightFocus,
          timeZone,
          svgRef,
          contextRef,
          focusRef,
        });
        // function brushed() {
        //   if (d3.event.sourceEvent && d3.event.sourceEvent.type === 'zoom')
        //     return; // ignore brush-by-zoom
        //   var s = d3.event.selection || xScaleContext.range();
        //   xScale.domain(s.map(xScaleContext.invert, xScaleContext));
        //   updateFocusLines({ s: focusLines, xScale, yScale });
        //   updateFocusXAxis({
        //     s: focusXAxis,
        //     getXAxis,
        //     xScale,
        //     tickVals,
        //     tickFmt,
        //     height: heightFocus,
        //     margin,
        //   });
        //   focusZoom.call(
        //     zoom.transform,
        //     d3.zoomIdentity.scale(width / (s[1] - s[0])).translate(-s[0], 0),
        //   );
        // }

        // function zoomed() {
        //   if (d3.event.sourceEvent && d3.event.sourceEvent.type === 'brush')
        //     return; // ignore zoom-by-brush
        //   var t = d3.event.transform;
        //   xScale.domain(t.rescaleX(xScaleContext).domain());
        //   updateFocusLines({ s: focusLines, xScale, yScale });
        //   updateFocusXAxis({
        //     s: focusXAxis,
        //     getXAxis,
        //     xScale,
        //     tickVals,
        //     tickFmt,
        //     height: heightFocus,
        //     margin,
        //   });
        //   updateContextBrush({
        //     brush,
        //     s: contextBrush,
        //     xScale,
        //   });
        // }

        // var brush = getBrush({ width, height: heightContext, brushed });
        // var zoom = getZoom({ width, height: heightFocus, zoomed });

        // // FocusZoom
        // const focusZoom = svg.selectAll('.zoom').data(['dummy']);
        // const contextBrush = context.selectAll('.brush').data(['dummy']);

        // updateZoom({ s: focusZoom, width, height: heightFocus, zoom, margin });
        // updateContextBrush({ brush, s: contextBrush, xScale });
      }
      setSpin(false);
    }
  }, [
    height,
    width,
    config,
    prices,
    events,
    svgRef.current,
    focusRef.current,
    contextRef.current,
  ]);

  return (
    <div
      ref={chartRef}
      id='chart-container'
      style={{ width: '100%', height: '100%' }}
    >
      <div
        id='chart'
        style={{
          width: '100%',
          height: '100%',
          textAlign: 'center',
        }}
      >
        {spin && <Spin size='large' style={{ margin: 'auto' }} />}
        <svg ref={svgRef}>
          <g className='focus' ref={focusRef} />
          <g className='context' ref={contextRef} />
        </svg>
      </div>
    </div>
  );
};

export default Chart;
