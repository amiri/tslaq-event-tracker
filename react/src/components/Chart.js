import React, { useState, useContext, useRef, useMemo } from 'react';
import { EventsContext } from '../contexts/EventsContext';
import { PricesContext } from '../contexts/PricesContext';
import { ChartContext } from '../contexts/ChartContext';
import moment from 'moment';
require('moment-timezone');
import useComponentSize from '@rehooks/component-size';
import { calculateDimensions } from './utils/Chart';
import { isEmpty, includes, isNil } from 'lodash';
import Focus from './Focus';
import Context from './Context';
import { Route, useHistory } from 'react-router-dom';
import EventsDetail from './EventsDetail';

const Chart = () => {
  const { events, setFilteredEvents } = useContext(EventsContext);
  const { prices } = useContext(PricesContext);
  const { config, setConfig } = useContext(ChartContext);
  const history = useHistory();

  const chartRef = useRef(null);
  const dimensions = useComponentSize(chartRef);
  const { height, width } = dimensions;
  const { margin, heightContext, heightFocus } = calculateDimensions({
    height,
    width,
  });
  const {
    timeZone,
    resolution,
    dateRange,
    categories,
    searchCondition,
  } = config;

  const priceList =
    prices && resolution === 'daily' ? prices.daily : prices.hourly;
  const timeField = resolution === 'daily' ? 'partialTime' : 'priceTime';

  const es = useMemo(
    () =>
      events
        ? events.map(e => {
            const et = moment.utc(e.time).tz('America/New_York');
            return Object.assign(e, { eventTime: et });
          })
        : null,
    [events],
  );
  const eventsByCategory = useMemo(
    () =>
      es
        ? es.reduce((obj, e) => {
            !isEmpty(e.categories) ?
                e.categories.map(c =>
                  obj[c.id] ? obj[c.id].add(e.id) : (obj[c.id] = new Set([e.id])),
                )
            : null
            return obj;
          }, {})
        : null,
    [es, categories],
  );

  const esFiltered = useMemo(
    () =>
      es
        ? es
            .filter(e =>
              isEmpty(dateRange)
                ? true
                : e.eventTime.isSameOrAfter(dateRange[0]) &&
                  e.eventTime.isSameOrBefore(dateRange[1]),
            )
            .filter(e =>
              isEmpty(categories)
                ? true
                : searchCondition === 'and'
                ? categories.every(c =>
                    !isNil(eventsByCategory[c])
                      ? includes(Array.from(eventsByCategory[c]), e.id)
                      : false,
                  )
                : categories.some(c =>
                    !isNil(eventsByCategory[c])
                      ? includes(Array.from(eventsByCategory[c]), e.id)
                      : false,
                  ),
            )
        : null,
    [es, dateRange, categories, searchCondition],
  );
  setFilteredEvents(esFiltered);

  const ps = useMemo(
    () =>
      priceList
        ? priceList.map(p => {
            const m =
              resolution === 'daily'
                ? moment(p[timeField])
                    .tz('America/New_York')
                    .tz(timeZone)
                : moment.utc(p[timeField]).tz(timeZone);
            return Object.assign(p, { priceTime: m });
          })
        : null,
    [priceList, timeField],
  );
  const psFiltered = useMemo(
    () =>
      ps
        ? ps.filter(p =>
            isEmpty(dateRange)
              ? true
              : p.priceTime.isSameOrAfter(dateRange[0]) &&
                p.priceTime.isSameOrBefore(dateRange[1]),
          )
        : null,
    [ps, dateRange],
  );

  const [zoomDomain, setZoomDomain] = useState([
    margin.left,
    width - margin.right,
  ]);

  const [brushDomain, setBrushDomain] = useState([
    margin.left,
    width - margin.right,
  ]);

  const onBrush = ({ xScale, range, eventType }) => {
    const newDomain = xScale ? range.map(xScale.invert, xScale) : null;
    const moments = newDomain
      ? newDomain.map(t => moment(t).tz('America/New_York'))
      : null;
    setConfig({
      ...config,
      ...(moments && { dateRange: moments }),
    });
    if (eventType) {
      setZoomDomain(range);
    }
  };

  const onZoom = ({ params }) => {
    const l = params[0] < margin.left ? margin.left : params[0];
    const r =
      params[1] > width - margin.right ? width - margin.right : params[1];
    setBrushDomain([l, r]);
  };

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
            ps={psFiltered}
            events={esFiltered}
            config={config}
            zoomF={onZoom}
            zoomDomain={zoomDomain}
            resolution={resolution}
            history={history}
          />
          <Context
            width={width}
            height={heightContext}
            margin={margin}
            ps={ps}
            config={config}
            brushF={onBrush}
            brushDomain={brushDomain}
          />
        </div>
      )}
      <Route
        path='/event'
        render={props => <EventsDetail {...props} events={esFiltered} />}
      />
    </div>
  );
};

export default Chart;
