import React, { useContext } from 'react';
import { EventsContext } from '../contexts/EventsContext';
import { PricesContext } from '../contexts/PricesContext';

const Chart = () => {
  const es = useContext(EventsContext);
  const ps = useContext(PricesContext);
  console.log(es);
  console.log(ps);
  return <div id='chart'>Here is the chart.</div>;
};

export default Chart;
