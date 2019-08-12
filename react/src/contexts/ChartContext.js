import React, { createContext, useState } from 'react';

export const ChartContext = createContext();

const s = {
  timeZone: 'America/New_York',
  margin: { top: 20, right: 20, bottom: 20, left: 20 },
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
