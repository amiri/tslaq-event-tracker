import React, * as react from 'react';
const fetch = require('node-fetch');

export const PricesContext = react.createContext();

const PricesContextProvider = props => {
  const [prices, setPrices] = react.useState({});

  async function getPrices() {
    const p = await window.api
      .getPrices()
      .then(res => res.data)
      .then(data => fetch(data.url))
      .then(res => res.json());
    setPrices({ daily: p.daily.partialPrices, hourly: p.hourly.prices });
  }

  react.useEffect(() => {
    getPrices();
  }, []);

  // Reload prices every hour
  react.useEffect(() => {
    const timer = setTimeout(() => {
      getPrices();
    }, 3600000);
    return () => clearTimeout(timer);
  });

  return (
    <PricesContext.Provider value={{ prices, setPrices }}>
      {props.children}
    </PricesContext.Provider>
  );
};

export default PricesContextProvider;
