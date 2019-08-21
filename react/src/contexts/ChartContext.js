import React, { createContext, useState } from 'react';

export const ChartContext = createContext();

const s = {
  timeZone: 'America/New_York',
  margin: { top: 25, right: 25, bottom: 25, left: 25 },
  resolution: 'daily',
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
