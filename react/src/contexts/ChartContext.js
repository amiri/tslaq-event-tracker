import React, { createContext, useState, useEffect, useReducer } from 'react';
import { categoriesReducer } from '../reducers/CategoriesReducer';

export const ChartContext = createContext();

const s = {
  timeZone: 'America/New_York',
  resolution: 'daily',
  dateRange: [],
  categories: [],
  // brushDomain: [],
  // zoomDomain: [],
};

const ChartContextProvider = props => {
  const [config, setConfig] = useState(s);
  const [categoryOptions, dispatch] = useReducer(categoriesReducer, []);

  async function getCategories() {
    await window.api.getCategories().then(res => dispatch({
        type: 'GET_CATEGORIES',
        payload: res.data,
    }),
    );
  }

  useEffect(() => {
    getCategories();
  }, []);

  // Reload categories every hour
  useEffect(() => {
    const timer = setTimeout(() => {
      getCategories();
    }, 3600000);
    return () => clearTimeout(timer);
  });
    return (
    <ChartContext.Provider value={{ config, setConfig, categoryOptions, dispatch }}>
      {props.children}
    </ChartContext.Provider>
  );
};
export default ChartContextProvider;
