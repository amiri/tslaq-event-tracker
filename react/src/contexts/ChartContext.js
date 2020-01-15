import React, { createContext, useState, useEffect, useReducer } from 'react';
import { categoriesReducer } from '../reducers/CategoriesReducer';
import { Select } from 'antd';
import {
  isArray,
  flattenDeep,
  isObject,
  mapKeys,
  mapValues,
  isNil,
  isEmpty,
  get,
  has,
  set,
} from 'lodash';

const { Option } = Select;

export const ChartContext = createContext();

const s = {
  timeZone: 'America/New_York',
  resolution: 'daily',
  dateRange: [],
  categories: [],
  searchCondition: 'and',
  // brushDomain: [],
  // zoomDomain: [],
};

const renameKeys = (names, obj) => {
  if (isArray(obj)) {
    return obj.map(inner => renameKeys(names, inner));
  } else if (isObject(obj)) {
    const res = mapKeys(obj, (v, k) => names[k] || k);
    return mapValues(res, v => renameKeys(names, v));
  } else {
    return obj;
  }
};

const transformOpt = obj => {
  return Object.keys(obj).reduce((os, k) => {
    if (k === 'direct') {
      os.push(
        obj[k]
          .sort((a, b) => (a.fullName > b.fullName ? 1 : -1))
          .map(o => (
            <Option key={o.id} value={o.id} label={o.name}>
              {o.fullName}
            </Option>
          )),
      );
    } else {
      os.push(transformOpt(obj[k]));
    }
    return os;
  }, []);
};

const ChartContextProvider = props => {
  const [config, setConfig] = useState(s);
  const [allCategories, dispatch] = useReducer(categoriesReducer, []);
  const [categoryOptions, setCategoryOptions] = useState([]);

  // Load category options when categories change
  useEffect(() => {
    if (!isEmpty(allCategories)) {
      let ns = {};
      const opts = allCategories.reduce((os, c) => {
        ns[c.id] = c.name;
        if (isNil(c.parents)) {
          const there = get(os, c.id, { direct: [] });
          there.direct.push(c);
          set(os, c.id, there);
        } else {
          const there = get(os, c.parents, { direct: [] });
          if (has(there, c.id)) {
            there[c.id].direct.push(c);
          } else {
            there.direct.push(c);
          }
          set(os, c.parents, there);
        }
        return os;
      }, {});
      const renamed = renameKeys(ns, opts);
      console.log('renamed: ', renamed);
      const categoryOptions = Object.keys(renamed).reduce((os, k) => {
        os.push(transformOpt(renamed[k]));
        return os;
      }, []);
      const flat = flattenDeep(categoryOptions);
      setCategoryOptions(flat);
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
      value={{ config, setConfig, allCategories, categoryOptions }}
    >
      {props.children}
    </ChartContext.Provider>
  );
};
export default ChartContextProvider;
