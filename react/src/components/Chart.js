import React, { useState, useContext, useRef, useMemo } from 'react';
import { EventsContext } from '../contexts/EventsContext';
import { PricesContext } from '../contexts/PricesContext';
import { ChartContext } from '../contexts/ChartContext';
import moment from 'moment';
require('moment-timezone');
import useComponentSize from '@rehooks/component-size';
import {
  calculateDimensions,
  updateQueryParams,
  sameDateRange,
  getQueryConfig,
} from './utils/Chart';
import {
  omit,
  mapValues,
  isEqual,
  isEmpty,
  includes,
  isNil,
  merge,
} from 'lodash';
import Focus from './Focus';
import Context from './Context';
import { Route } from 'react-router-dom';
import EventsDetail from './EventsDetail';
import { margin } from './utils/Chart';

const Chart = props => {
  const { events, setFilteredEvents } = useContext(EventsContext);
  const { prices } = useContext(PricesContext);
  const { config, setConfig } = useContext(ChartContext);
  const { history, location } = props;
  const queryConfig = getQueryConfig({ location });

  const chartRef = useRef(null);
  const dimensions = useComponentSize(chartRef);
  const { height, width } = dimensions;

  const { heightContext, heightFocus } = useMemo(
    () =>
      calculateDimensions({
        height,
        width,
      }),
    [height, width],
  );

  if (!isEmpty(queryConfig)) {
    const merged = merge({}, config, queryConfig);
    if (
      sameDateRange({ orig: config.dateRange, updates: queryConfig.dateRange })
    ) {
      if (!isEqual(omit(merged, ['dateRange']), omit(config, ['dateRange']))) {
        setConfig(merged);
      }
    } else {
      if (!isEqual(merged, config)) {
        setConfig(merged);
      }
    }
  }

  const {
    timeZone,
    resolution,
    dateRange,
    categories,
    searchCondition,
    searchSubcategories,
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
            !isEmpty(e.categories)
              ? e.categories.map(c => {
                  if (JSON.parse(searchSubcategories)) {
                    const cats = [c.id, ...(c.parents ? c.parents : [])];
                    cats.map(cat =>
                      obj[cat]
                        ? obj[cat].add(e.id)
                        : (obj[cat] = new Set([e.id])),
                    );
                  } else {
                    obj[c.id]
                      ? obj[c.id].add(e.id)
                      : (obj[c.id] = new Set([e.id]));
                  }
                })
              : null;
            return obj;
          }, {})
        : null,
    [es, categories, searchSubcategories],
  );

  const esFiltered = useMemo(
    () =>
      es
        ? es
            .filter(e =>
              isEmpty(dateRange)
                ? true
                : e.eventTime.isSameOrAfter(dateRange.startDate) &&
                  e.eventTime.isSameOrBefore(dateRange.endDate),
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
              : p.priceTime.isSameOrAfter(dateRange.startDate) &&
                p.priceTime.isSameOrBefore(dateRange.endDate),
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

  const onBrush = ({ xScale, range }) => {
    const newDomain = xScale ? range.map(xScale.invert, xScale) : null;
    const moments = newDomain
      ? newDomain.map(t => moment(t).tz('America/New_York'))
      : null;
    const dateRange = { startDate: moments[0], endDate: moments[1] };
    const params = mapValues(dateRange, v => {
      return v.format('YYYY-MM-DD');
    });
    updateQueryParams({ params, history, location });
    if (!isEqual(range, zoomDomain)) {
      setZoomDomain(range);
    }
  };

  const onZoom = ({ params, eventType }) => {
    const l = params[0] < margin.left ? margin.left : params[0];
    const r =
      params[1] > width - margin.right ? width - margin.right : params[1];
    const checkedR = ((r - l) < 5) ? l + 5 : r;
    if (eventType === 'wheel' || eventType === 'mousemove') {
      if (!isEqual([l, checkedR], brushDomain)) {
        setBrushDomain([l, checkedR]);
      }
    }
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
            ps={psFiltered}
            events={esFiltered}
            config={config}
            zoomF={onZoom}
            zoomDomain={zoomDomain}
            resolution={resolution}
            history={history}
            location={location}
          />
          <Context
            width={width}
            height={heightContext}
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
