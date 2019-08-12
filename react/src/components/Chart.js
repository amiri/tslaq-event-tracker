/* eslint-disable no-unused-vars */
import React, { useContext, useEffect, useState, useRef } from 'react';
import { EventsContext } from '../contexts/EventsContext';
import { PricesContext } from '../contexts/PricesContext';
import { ChartContext } from '../contexts/ChartContext';
import moment from 'moment';
import * as d3 from 'd3';
import { isNil } from 'lodash';
import { Spin } from 'antd';
require('moment-timezone');

const Chart = () => {
  const [dimensions, setDimensions] = useState({ height: null, width: null });

  const chartRef = useRef();

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

  const { margin, timeZone } = config;
  const { height, width } = dimensions;

  // convert dates
  const ps = prices.map(p => {
    const m = moment.utc(p.priceTime).tz(timeZone);
    return Object.assign(p, { priceTime: m });
  });

  // scales
  const xExtent = d3.extent(ps, p => p.priceTime);
  const yExtent = d3.extent(ps, p => p.high);

  if (!isNil(xExtent[0])) {
    console.log(xExtent);
  }

  const xScale =
    !isNil(xExtent[0]) && !isNil(xExtent[1]) && !isNil(width)
      ? d3
          .scaleBand()
          .domain(
            d3.timeDay.range(xExtent[0].toDate(), xExtent[1].toDate())
            // .filter(d => {
            //   const est = moment(d).tz('America/New_York');
            //   const test = est.day() !== 0 && est.day() !== 6 ? true : false;
            //     return test;
            // }),
          )
          .range([margin.left, width - margin.right])
      : null;

  if (xScale) {
    // console.log(xScale(new Date("Mon Jul 14 2014 06:00:00 GMT-0700 (Pacific Daylight Time)")));
    console.log(
      xScale(
        new Date('Sat Aug 03 2019 09:00:00 GMT-0700 (Pacific Daylight Time)'),
      ),
    );
    console.log(
      xScale(
        new Date('Tue Aug 06 2019 09:00:00 GMT-0700 (Pacific Daylight Time)'),
      ),
    );
  }

  const yScale =
    !isNil(yExtent[0]) && !isNil(yExtent[1]) && !isNil(height)
      ? d3
          .scaleLinear()
          .domain([0, yExtent[1]])
          .range([0, height - margin.bottom, margin.top])
      : null;

  const halfHeight = Math.round(height / 2);

  return (
    <div ref={chartRef} id='chart' style={{ width: '100%', height: '100%' }}>
      {ps.length ? (
        <svg width={width} height={height}></svg>
      ) : (
        <div
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
      )}
    </div>
  );
};

export default Chart;
