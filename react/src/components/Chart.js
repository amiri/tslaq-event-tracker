/* eslint-disable no-unused-vars, no-inner-declarations */
import React, { useContext, useEffect, useState, useRef } from 'react';
import { EventsContext } from '../contexts/EventsContext';
import { PricesContext } from '../contexts/PricesContext';
import { ChartContext } from '../contexts/ChartContext';
import moment from 'moment';
import * as d3 from 'd3';
import { isNil, isEmpty } from 'lodash';
import { Spin } from 'antd';
require('moment-timezone');
import { timeInterval, durationMinute, durationWeek } from 'd3-time';

const Chart = () => {
  const [dimensions, setDimensions] = useState({ height: null, width: null });

  const chartRef = useRef();

  // Grab height and width from browser
  useEffect(() => {
    if (chartRef.current) {
      setDimensions({
        width: chartRef.current.offsetWidth - 7,
        height: chartRef.current.offsetHeight - 7,
      });
    }
  }, []);

  const { events } = useContext(EventsContext);
  const { prices } = useContext(PricesContext);
  const { config } = useContext(ChartContext);

  // Draw chart, now that we have dimensions and contextual data loaded.
  useEffect(() => {
    const { margin, timeZone, resolution } = config;
    const { height, width } = dimensions;
    const priceList =
      resolution === 'daily' && !isNil(prices) ? prices.daily : prices.hourly;
    const getXScale = ({ xExtent, width, margin }) =>
      d3
        .scaleBand()
        .domain(
          d3.timeDay
            .range(xExtent[0].toDate(), +xExtent[1].toDate() + 1)
            .filter(d => {
              const est = moment(d).tz('America/New_York');
              return est.dayOfYear(1) || (est.day() !== 0 && est.day() !== 6);
            }),
        )
        .range([margin.left, width - margin.right]);

    const getYScale = ({ yExtent, height, margin }) =>
      d3
        .scaleLinear()
        .domain([0, yExtent[1]])
        .range([height - margin.bottom, margin.top]);

    if (
      !isNil(height) &&
      !isNil(width) &&
      !isNil(priceList) &&
      priceList.length
    ) {
      // convert dates
      const timeField = resolution == 'daily' ? 'partialTime' : 'priceTime';
      const ps = priceList.map(p => {
        const m =
          resolution === 'daily'
            ? moment(p[timeField])
                .tz('America/New_York')
                .tz(timeZone)
            : moment.utc(p[timeField]).tz(timeZone);
        return Object.assign(p, { priceTime: m });
      });

      // Function for xAxis intervals
      const timeMondayEST = () =>
        timeInterval(
          function(date) {
            const est = moment(date)
              .tz('America/New_York')
              .toDate();
            date.setDate(est.getDate() - ((est.getDay() + 7 - 1) % 7));
            date.setHours(0, 0, 0, 0);
          },
          function(date, step) {
            const est = moment(date)
              .tz('America/New_York')
              .toDate();
            date.setDate(est.getDate() + step * 7);
          },
          function(start, end) {
            return (
              (end -
                start -
                (end.getTimezoneOffset() - start.getTimezoneOffset()) *
                  durationMinute) /
              durationWeek
            );
          },
        );

      // scales
      const xExtent = d3.extent(ps, p => p.priceTime);
      const yExtent = d3.extent(ps, p => p.high);
      const xScale = getXScale({ xExtent, width, margin });
      const yScale = getYScale({ yExtent, height, margin });

      const tickVals = d3.timeMonth
        .range(xExtent[0].toDate(), xExtent[1].toDate())
        .filter(d => d.getMonth() % 3 === 0);

      const getXAxis = g => {
        g.attr('transform', `translate(0,${height - margin.bottom})`)
          .call(
            d3
              .axisBottom(xScale)
              .tickValues(tickVals)
              .tickFormat(d =>
                d <= d3.timeYear(d) ? d3.timeFormat('%Y')(d) : null,
              ),
          )
          .call(g => g.select('.domain').remove());
      };
      const getYAxis = g => {
        g.attr('transform', `translate(${margin.left},0)`)
          .call(
            d3.axisRight(yScale).tickSize(width - margin.left - margin.right),
          )
          .call(g => g.select('.domain').remove())
          .call(g =>
            g
              .selectAll('.tick:not(:first-of-type) line')
              .attr('stroke-opacity', 0.15)
              .attr('stroke-dasharray', '2,2'),
          )
          .call(g =>
            g
              .selectAll('.tick text')
              .attr('x', 4)
              .attr('dy', -4),
          );
      };

      // CODE HERE

      d3.select('#chart').remove();
      const svg = d3
        .select('#chart-container')
        .append('svg')
        .attr('preserveAspectRatio', 'xMinYMin meet')
        .attr('viewBox', `0 0 ${width} ${height}`);
      svg.append('g').call(getXAxis);
      svg.append('g').call(getYAxis);
      svg
        .append('g')
        .attr('transform', `translate(0, ${yScale(yExtent[1])})`)
        .append('line')
        .attr('x2', width)
        .style('stroke', '#85bb65')
        .style('stroke-width', '1px');
      svg
        .append('g')
        .attr('transform', `translate(0, ${yScale(yExtent[0])})`)
        .append('line')
        .attr('x2', width)
        .style('stroke', '#8A0707')
        .style('stroke-width', '1px');

      const candle = svg
        .append('g')
        .attr('stroke-linecap', 'square')
        .attr('stroke', 'black')
        .selectAll('g')
        .data(ps)
        .join('g')
        .attr('transform', d => `translate(${xScale(d.priceTime.toDate())},0)`);
      candle
        .append('line')
        .attr('y1', d => yScale(d.low))
        .attr('y2', d => yScale(d.high));
      candle
        .append('line')
        .attr('y1', d => yScale(d.open))
        .attr('y2', d => yScale(d.close))
        .attr('stroke', (d,i) => {
          console.log(i);
          return d.open > d.close
            ? d3.schemeSet1[0]
            : d.close > d.open
            ? d3.schemeSet1[2]
            : d3.schemeSet1[8];
        })
        .attr('stroke-width', xScale.bandwidth());
    }
  }, [dimensions, config, events, prices, config]);

  const halfHeight = dimensions.height ? Math.round(dimensions.height / 2) : 0;

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
          padding: '5%',
        }}
      >
        <Spin
          size='large'
          style={{ height: '`${halfHeight}px`', margin: 'auto' }}
        />
      </div>
    </div>
  );
};

export default Chart;
