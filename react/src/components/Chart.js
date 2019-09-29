/* eslint-disable no-unused-vars, no-inner-declarations */
import React, {
  useLayoutEffect,
  useContext,
  useEffect,
  useState,
  useRef,
} from 'react';
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
  getLines,
  getTickVals,
  getXAxis,
  getXScale,
  scaleBandInvert,
  getYAxis,
  getYScale,
  updateClipPath,
  updateContextBrush,
  updateContextLines,
  updateContextXAxis,
  updateFocusLines,
  updateFocusXAxis,
  updateFocusYAxis,
  updateZoom,
  transformZoom,
  getBrushF,
  getZoomF,
  xBand,
} from './utils/Chart';

const Chart = () => {
  const { events } = useContext(EventsContext);
  const { prices } = useContext(PricesContext);
  const { config } = useContext(ChartContext);
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
      const {
        totalHeightContext,
        totalHeightFocus,
        margin,
        heightContext,
        heightFocus,
      } = calculateDimensions({ height, width });

      const priceList = resolution === 'daily' ? prices.daily : prices.hourly;
      const timeField = resolution == 'daily' ? 'partialTime' : 'priceTime';

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
        console.log(heightContext);
        console.log(heightFocus);
        // Extents
        const xExtent = d3.extent(ps, p => p.priceTime);
        const yExtent = d3.extent(ps, p => p.high);

        // Scales
        const xScale = getXScale({ xExtent, width, margin });
        const xScaleContext = getXScale({ xExtent, width, margin });

        const yScale = getYScale({ yExtent, height: heightFocus, margin });
        const yScaleContext = getYScale({
          yExtent,
          height: heightContext,
          margin,
        });

        // Tick values
        const { tickVals, tickFmt } = getTickVals({ xExtent, timeZone });

        // Size svg and g refs
        const svg = d3
          .select(svgRef.current)
          .attr('preserveAspectRatio', 'xMinYMin meet')
          .attr('viewBox', `0 0 ${width ? width : 0} ${height ? height : 0}`);

        const focus = d3
          .select(focusRef.current)
          .attr('transform', `translate(0, ${margin.top})`);

        const context = d3
          .select(contextRef.current)
          .attr(
            'transform',
            `translate(0, ${heightFocus + margin.bottom + margin.top})`,
          );

        // ClipPath
        const clipPath = svg.selectAll('defs').data(['dummy']);
        updateClipPath({ s: clipPath, width, height: heightFocus, margin });

        // ContextLines
        const contextLines = context.selectAll('.line').data([ps]);
        updateContextLines({
          s: contextLines,
          xScale,
          yScale: yScaleContext,
        });

        // FocusLines
        const focusLines = focus.selectAll('.line').data([ps]);
        updateFocusLines({ s: focusLines, xScale, yScale });

        // FocusXAxis
        const focusXAxis = focus.selectAll('.x-axis').data(['dummy']);
        updateFocusXAxis({
          s: focusXAxis,
          getXAxis,
          xScale,
          tickVals,
          tickFmt,
          height: heightFocus,
          margin,
        });

        // FocusYAxis
        const focusYAxis = focus.selectAll('.y-axis').data(['dummy']);
        updateFocusYAxis({ s: focusYAxis, margin, yScale, width });

        // ContextXAxis
        const contextXAxis = context.selectAll('.x-axis').data(['dummy']);
        updateContextXAxis({
          s: contextXAxis,
          xScale,
          tickVals,
          tickFmt,
          height: heightContext + margin.bottom,
          margin,
        });

        var brush = d3
          .brushX()
          .extent([[0, 0], [width, heightContext]])
          .on('brush end', brushed);

        var zoom = d3
          .zoom()
          .scaleExtent([1, Infinity])
          .translateExtent([[0, 0], [width, heightFocus]])
          .extent([[0, 0], [width, heightFocus]])
          .on('zoom', zoomed);

        // FocusZoom
        const focusZoom = svg.selectAll('.zoom').data(['dummy']);
        const contextBrush = context.selectAll('.brush').data(['dummy']);

        function brushed() {
          if (d3.event.sourceEvent && d3.event.sourceEvent.type === 'zoom')
            return; // ignore brush-by-zoom
          var s = d3.event.selection || xScaleContext.range();
          const x2 = d3
            .scaleBand()
            .domain(s.map(scaleBandInvert(xScaleContext), xScaleContext))
            .range([margin.left, width - margin.right]);
          updateFocusLines({ s: focusLines, xScale: x2, yScale });
          updateFocusXAxis({
            s: focusXAxis,
            getXAxis,
            xScale: x2,
            tickVals,
            tickFmt,
            height: heightFocus,
            margin,
          });
          focusZoom.call(
            zoom.transform,
            d3.zoomIdentity.scale(width / (s[1] - s[0])).translate(-s[0], 0),
          );
        }

        function zoomed() {
          if (d3.event.sourceEvent && d3.event.sourceEvent.type === 'brush')
            return; // ignore zoom-by-brush
          // Make new xScale
          // Move brush to new xScale range
          // Update FocusLines
          // Update FocusXAxis
        }

        updateZoom({ s: focusZoom, width, height: heightFocus, zoom, margin });
        updateContextBrush({
          brush,
          s: contextBrush,
          xScale,
        });

        setSpin(false);
      }
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
