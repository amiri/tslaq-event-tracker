import React, { createContext, useState, useEffect, useReducer } from 'react';
import { categoriesReducer } from '../reducers/CategoriesReducer';
import { Select } from 'antd';
import { isEmpty } from 'lodash';
import {
  renameKeys,
  transformOpt,
  nestCategories,
} from '../components/utils/Chart';

const { OptGroup } = Select;

export const ChartContext = createContext();

const s = {
  timeZone: 'America/New_York',
  resolution: 'daily',
  dateRange: {},
  categories: [],
  searchCondition: 'and',
  // brushDomain: [],
  // zoomDomain: [],
};

const ChartContextProvider = props => {
  // Chart configuration
  const [config, setConfig] = useState(s);
  // All categories, in list form
  const [allCategories, dispatch] = useReducer(categoriesReducer, []);
  // Categories in optgroup/option form
  const [categoryOptions, setCategoryOptions] = useState([]);
  // Reverse lookup for options
  const [valuePerOptionName, setValuePerOptionName] = useState({});

  // Load category options when categories change
  useEffect(() => {
    if (!isEmpty(allCategories)) {
      const [nested, ns] = nestCategories(allCategories);
      const renamed = renameKeys(ns, nested);
      const categoryOptions = Object.keys(renamed).reduce((os, k) => {
        os.push(
          <OptGroup label={k} key={k}>
            {transformOpt(renamed[k])}
          </OptGroup>,
        );
        return os;
      }, []);
      // SET options
      setCategoryOptions(categoryOptions);
      const lowerCaseNs = Object.fromEntries(
        Object.entries(ns).map(([k, v]) => [v.toLowerCase(), k]),
      );
      // SET reverse lookup
      setValuePerOptionName(lowerCaseNs);
    }
  }, [allCategories]);

  async function getCategories() {
    await window.api.getCategories().then(res =>
      dispatch({
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
    <ChartContext.Provider
      value={{
        config,
        setConfig,
        allCategories,
        categoryOptions,
        valuePerOptionName,
      }}
    >
      {props.children}
    </ChartContext.Provider>
  );
};
export default ChartContextProvider;
