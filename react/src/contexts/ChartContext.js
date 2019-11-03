import React, { createContext, useState } from 'react';

export const ChartContext = createContext();

const s = {
  timeZone: 'America/New_York',
  resolution: 'daily',
  dateRange: [],
  // brushDomain: [],
  // zoomDomain: [],
};

const ChartContextProvider = props => {
  const [config, setConfig] = useState(s);
  return (
    <ChartContext.Provider value={{ config, setConfig }}>
      {props.children}
    </ChartContext.Provider>
  );
};
export default ChartContextProvider;
