import React, { useContext, useRef } from 'react';
import { EventsContext } from '../contexts/EventsContext';
import { PricesContext } from '../contexts/PricesContext';
import { ChartContext } from '../contexts/ChartContext';
import moment from 'moment';
require('moment-timezone');
import useComponentSize from '@rehooks/component-size';
import { calculateDimensions } from './utils/Chart';
import { isEmpty } from 'lodash';
import Focus from './Focus';
import Context from './Context';

const Chart = () => {
  const { events } = useContext(EventsContext);
  const { prices } = useContext(PricesContext);
  const { config } = useContext(ChartContext);

  const chartRef = useRef(null);
  const dimensions = useComponentSize(chartRef);
  const { height, width } = dimensions;
  const { margin, heightContext, heightFocus } = calculateDimensions({
    height,
    width,
  });
  const { timeZone, resolution, dateRange } = config;

  const priceList =
    prices && resolution === 'daily' ? prices.daily : prices.hourly;
  const timeField = resolution === 'daily' ? 'partialTime' : 'priceTime';

  const ps = priceList
    ? priceList
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
        )
    : null;

  const onBrush = ({ xScale, range }) => {
    console.log('Brush data: ', range);
    const newDomain = range.map(xScale.invert, xScale);
    console.log('New domain: ', newDomain);
  };

  // const onZoom = d => {
  //   console.log('Brush data: ', d);
  // };

  return (
    <div
      ref={chartRef}
      id='chart-container'
      style={{ width: '100%', height: '100%' }}
    >
      {!isEmpty(ps) && (
        <div
          id='chart'
          style={{
            width: '100%',
            height: '100%',
            textAlign: 'center',
          }}
        >
          <Focus
            width={width}
            height={heightFocus}
            margin={margin}
            ps={ps}
            events={events}
            config={config}
          />
          <Context
            width={width}
            height={heightContext}
            focusHeight={heightFocus}
            margin={margin}
            ps={ps}
            config={config}
            brushFn={onBrush}
          />
        </div>
      )}
    </div>
  );
};

export default Chart;
