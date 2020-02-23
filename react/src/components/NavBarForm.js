import React, { useEffect } from 'react';
import { Formik } from 'formik';
import { Spin, Form, Switch, Select, Button, Radio, DatePicker } from 'antd';
import { openCategoryModal } from './utils/Chart';
import { isEmpty } from 'lodash';
const { RangePicker } = DatePicker;
import ReactGA from 'react-ga';

const radioStyle = {
  display: 'block',
};

const NavBarForm = ({
  config,
  categoryOptions,
  history,
  updateCategories,
  updateRange,
  updateSearchCondition,
  viewEvents,
  valuePerOptionName,
  updateSearchSubcategories,
  location,
}) => {
  const newCategory = sessionStorage.getItem('newCategoryChoice')
    ? sessionStorage.getItem('newCategoryChoice')
    : null;

  useEffect(() => {
    if (newCategory) {
      updateCategories([...config.categories, valuePerOptionName[newCategory]]);
      sessionStorage.removeItem('newCategoryChoice');
    }
  }, [newCategory]);

  return (
    <Formik
      initialValues={{
        categories: config.categories,
        searchCondition: config.searchCondition,
        dateRange: config.dateRange,
        searchSubcategories: config.searchSubcategories,
      }}
    >
      {({ isSubmitting }) => (
        <Form
          layout='inline'
          onSubmit={e => {
            ReactGA.event({
              category: 'Form',
              action: 'ViewEvents',
              transport: 'beacon',
            });
            e.preventDefault();
            viewEvents({ history });
          }}
        >
          <Form.Item>
            <Select
              size='small'
              style={{
                width: '100%',
                marginRight: '1em',
                minWidth: '24em',
                maxWidth: '32em',
              }}
              allowClear={true}
              mode='multiple'
              value={config.categories}
              onDeselect={e => {
                const settable = config.categories.filter(v => v !== e);
                updateCategories(settable);
              }}
              onSelect={e => {
                if (/^parent-/.test(e.toString())) {
                  openCategoryModal({ history, option: e, location });
                }
              }}
              placeholder='Safety, Model 3'
              onChange={values => {
                const settable = values.filter(
                  v => !/^parent-/.test(v.toString()),
                );
                updateCategories(settable);
              }}
            >
              {categoryOptions}
            </Select>
          </Form.Item>
          <Form.Item>
            <div style={{ display: 'flex' }}>
              <Radio.Group
                size='small'
                onChange={updateSearchCondition}
                value={config.searchCondition}
              >
                <Radio size='small' style={radioStyle} value='and'>
                  and
                </Radio>
                <Radio size='small' style={radioStyle} value='or'>
                  or
                </Radio>
              </Radio.Group>
            </div>
          </Form.Item>
          <Form.Item>
            <Switch
              size='small'
              checked={JSON.parse(config.searchSubcategories)}
              value={JSON.parse(config.searchSubcategories)}
              onChange={updateSearchSubcategories}
              checkedChildren='Search subcategories'
              unCheckedChildren='Specified only'
            />
          </Form.Item>
          <Form.Item>
            <RangePicker
              size='small'
              allowClear={true}
              onChange={dates => updateRange(dates)}
              value={
                !isEmpty(config.dateRange)
                  ? [config.dateRange.startDate, config.dateRange.endDate]
                  : null
              }
            />
          </Form.Item>
          <Form.Item>
            <Button size='small' type='primary' htmlType='submit'>
              View Events
            </Button>
          </Form.Item>
          {isSubmitting && (
            <Form.Item>
              <Spin />
            </Form.Item>
          )}
        </Form>
      )}
    </Formik>
  );
};

export default NavBarForm;
