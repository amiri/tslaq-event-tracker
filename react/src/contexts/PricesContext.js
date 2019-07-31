import React, { createContext, useState, useEffect } from 'react';
const fetch = require('node-fetch');

export const PricesContext = createContext();

const PricesContextProvider = props => {
  const [prices, setPrices] = useState([]);

  async function getPrices() {
    const p = await window.api
      .getPrices()
      .then(res => res.data)
      .then(data => fetch(data.url))
      .then(res => res.json());
    setPrices(p);
  }

  useEffect(() => {
    getPrices();
  }, []);

  // Reload prices every hour
  useEffect(() => {
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
