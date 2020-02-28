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
  searchSubcategories: false,
};

const ChartContextProvider = props => {
  // Chart configuration
  const [config, setConfig] = useState(s);

  // All categories, in list form
  const [allCategories, dispatch] = useReducer(categoriesReducer, []);

  // Categories in optgroup/option form
  const [categoryOptions, setCategoryOptions] = useState([]);

  // Lookup full name for option value
  const [fullNamePerOptionValue, setFullNamePerOptionValue] = useState({});

  // Lookup value for option name
  const [valuePerOptionName, setValuePerOptionName] = useState({});

  // Lookup value for option full name
  const [valuePerOptionFullName, setValuePerOptionFullName] = useState({});

  // Load category options when categories change
  useEffect(() => {
    if (!isEmpty(allCategories)) {
      const [nested, nsFull, ns] = nestCategories(allCategories);
      const reverseNsFull = Object.fromEntries(
        Object.entries(nsFull).map(([k, v]) => [v, k]),
      );
      const renamed = renameKeys(nsFull, nested);
      const categoryOptions = Object.keys(renamed)
        .sort()
        .reduce((os, k) => {
          os.push(
            <OptGroup label={k} key={k}>
              {transformOpt(renamed[k], k, reverseNsFull[k], reverseNsFull)}
            </OptGroup>,
          );
          return os;
        }, []);
      // SET options
      setCategoryOptions(categoryOptions);
      // SET lookups
      setFullNamePerOptionValue(nsFull);
      setValuePerOptionName(ns);
      setValuePerOptionFullName(reverseNsFull);
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
        valuePerOptionFullName,
        fullNamePerOptionValue,
        dispatch,
      }}
    >
      {props.children}
    </ChartContext.Provider>
  );
};
export default ChartContextProvider;
