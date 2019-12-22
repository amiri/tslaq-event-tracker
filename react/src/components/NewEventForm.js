import * as Yup from 'yup';
import moment from 'moment';
import { EventsContext } from '../contexts/EventsContext';
import React, { useContext } from 'react';
import * as alerts from '../alerts';
import { Formik } from 'formik';
import { difference, includes } from 'lodash';
import { Form, Input, Select, Button, Spin, DatePicker } from 'antd';

const EventSchema = Yup.object().shape({
  body: Yup.string().required('You must enter the text of the event.'),
  time: Yup.date().required('You must enter the event date and time.'),
  title: Yup.string().required('You must enter the title of the event.'),
  categories: Yup.array()
    .required('You must choose at least one category.')
    .of(Yup.string().min(1)),
});

const transformApiError = ({ statusText, data }) => {
  if (
    data.detail ===
    'Error in $.categories: parsing NonEmpty failed, unexpected empty list'
  ) {
    return { categories: 'You must choose at least one category.' };
  }
  if (data.title === 'EventConflict') {
    return { title: data.detail };
  }
};

const markNewCategories = ({ values, options }) => {
  const newCategories = difference(values, options);
  const updated = values.filter(v => !includes(newCategories, v));
  const newMarked = newCategories.map(c => `newcat-${c}`);
  return [...updated, ...newMarked];
};

const { TextArea } = Input;
const NewEventForm = ({ event, categoryOptions }) => {
  const { dispatch } = useContext(EventsContext);
  return (
    <Formik
      initialValues={{
        body: '',
        time: '',
        title: '',
        categories: [],
        ...event,
      }}
      onSubmit={async (values, actions) => {
        const eventData = {
          body: values.body,
          time: values.time,
          title: values.title,
          categories: markNewCategories({
            values: values.categories,
            options: categoryOptions.map(o => o.key),
          }),
        };
        await window.api
          .postEvents(eventData)
          .then(res => res.data)
          .then(data => {
            dispatch({
              type: 'POST_EVENT',
              payload: data,
            });
            actions.setSubmitting(false);
            alerts.success(`Event ${data.title} created`);
          })
          .catch(apiError => {
            actions.setSubmitting(false);
            const transformedError = transformApiError(apiError);
            actions.setErrors(transformedError);
          });
      }}
      validateOnBlur={false}
      validateOnChange={false}
      validationSchema={EventSchema}
      render={({
        values,
        errors,
        handleBlur,
        handleChange,
        handleSubmit,
        isSubmitting,
        setFieldValue,
      }) => (
        <Form onSubmit={handleSubmit}>
          <Form.Item
            validateStatus={errors && errors.time ? 'error' : ''}
            help={errors && errors.time ? errors.time : ''}
          >
            <DatePicker
              size='small'
              showTime
              defaultValue={moment(values.time)}
            />
          </Form.Item>
          <Form.Item
            validateStatus={errors && errors.title ? 'error' : ''}
            help={errors && errors.title ? errors.title : ''}
          >
            <Input
              type='text'
              placeholder='Some Event Happened'
              onChange={handleChange}
              onBlur={handleBlur}
              value={values.title}
              name='title'
              size='small'
            />
          </Form.Item>
          <Form.Item
            validateStatus={errors && errors.body ? 'error' : ''}
            help={errors && errors.body ? errors.body : ''}
          >
            <TextArea
              rows={10}
              type='text'
              placeholder=''
              onChange={handleChange}
              onBlur={handleBlur}
              value={values.body}
              name='body'
              size='small'
            />
          </Form.Item>
          <Form.Item
            validateStatus={errors && errors.categories ? 'error' : ''}
            help={errors && errors.categories ? errors.categories : ''}
          >
            <Select
              combobox
              mode='tags'
              placeholder='Safety, Model 3'
              onChange={e => {
                setFieldValue('categories', e);
              }}
              onBlur={handleBlur}
              name='categories'
              size='small'
            >
              {categoryOptions}
            </Select>
          </Form.Item>
          <Form.Item>
            <Button size='small' type='primary' htmlType='submit'>
              Create
            </Button>
          </Form.Item>
          {isSubmitting && (
            <Form.Item>
              <Spin />
            </Form.Item>
          )}
        </Form>
      )}
    />
  );
};

export default NewEventForm;
