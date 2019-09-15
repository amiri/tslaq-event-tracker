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
  getXScale,
  getYScale,
  calculateDimensions,
  getXAxis,
  getYAxis,
  getTickVals,
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
      console.log('ps: ', ps);
      if (!isEmpty(dateRange)) {
        console.log(
          ps[0].priceTime.isSameOrAfter(dateRange[0]) &&
            ps[0].priceTime.isSameOrBefore(dateRange[1]),
        );
      }

      if (!isEmpty(ps)) {
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
        console.log(moment.duration(xExtent[1].diff(xExtent[0])));

        const { tickVals, tickFmt } = getTickVals({ xExtent, timeZone });

        console.log(tickVals);
        console.log('heightFocus: ', heightFocus);
        console.log('heightContext: ', heightContext);

        const svg = d3
          .select(svgRef.current)
          .attr('preserveAspectRatio', 'xMinYMin meet')
          .attr('viewBox', `0 0 ${width ? width : 0} ${height ? height : 0}`);

        const focus = d3.select(focusRef.current);
        const context = d3.select(contextRef.current);

        // Focus
        const focusXAxis = focus.selectAll('.x-axis').data(['dummy']);

        focusXAxis
          .enter()
          .append('g')
          .attr('class', 'x-axis')
          .merge(focusXAxis)
          .call(getXAxis, {
            xScale,
            tickVals,
            tickFmt,
            height: heightFocus,
            margin,
          });

        const focusYAxis = focus.selectAll('.y-axis').data(['dummy']);

        focusYAxis
          .enter()
          .append('g')
          .attr('class', 'y-axis')
          .merge(focusYAxis)
          .call(getYAxis, { yScale, margin, width });

        // Context Enter
        const contextXAxis = context.selectAll('.x-axis').data(['dummy']);

        contextXAxis
          .enter()
          .append('g')
          .attr('class', 'x-axis')
          .merge(contextXAxis)
          .call(getXAxis, {
            xScale,
            tickVals,
            tickFmt,
            height: heightFocus + heightContext + margin.bottom,
            margin,
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
