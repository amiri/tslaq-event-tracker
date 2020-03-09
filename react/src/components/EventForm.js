import * as Yup from 'yup';
import moment from 'moment';
import { EventsContext } from '../contexts/EventsContext';
import React, { useContext } from 'react';
import * as alerts from '../alerts';
import { Formik } from 'formik';
import { has, compact, isArray } from 'lodash';
import { Form, Input, Select, Button, Spin, DatePicker } from 'antd';
import Qeditor from './Qeditor';
import {
  optionAddNewCategory,
  openNewCategoryModal,
  getEventEdits,
} from './utils/Chart';
import ReactGA from 'react-ga';
import * as QueryString from 'query-string';

const transformApiError = ({ data }) => {
  if (
    data.detail ===
    'Error in $.categories: parsing NonEmpty failed, unexpected empty list'
  ) {
    return { categories: 'You must choose at least one category.' };
  }
  if (data.title === 'EventConflict') {
    return { title: data.detail };
  }
  if (/Categor/.test(data.title)) {
    return { categories: data.detail };
  }
  if (data.title === 'WrongAuthor') {
    return {
      title: data.detail,
      body: data.detail,
      time: data.detail,
      categories: data.detail,
    };
  }
};

const EventSchema = Yup.object().shape({
  body: Yup.string().required('You must enter the text of the event.'),
  time: Yup.date().required('You must enter the event date and time.'),
  title: Yup.string().required('You must enter the title of the event.'),
  categories: Yup.array()
    .required('You must choose at least one category.')
    .of(Yup.string().min(1)),
});

const retrieveStoredEvent = () =>
  JSON.parse(sessionStorage.getItem('eventEditing'));

const EventForm = ({
  setVisible,
  event = retrieveStoredEvent(),
  categoryOptions: children,
  valuePerOptionName,
  history,
  location,
}) => {
  const { dispatch } = useContext(EventsContext);
  const params = QueryString.parse(location.search);

  const editMode = event || params.id ? true : false;
  // console.log('EventForm: event before Formik: ', event);
  // console.log('EventForm: event before Formik: ', event);

  return (
    <Formik
      enableReinitialize={true}
      initialValues={{
        body: has(event, 'body')
          ? JSON.parse(event.body)
          : [
              {
                type: 'paragraph',
                children: [{ text: 'Start here.' }],
              },
            ],
        time: has(event, 'time') ? event.time : '',
        title: has(event, 'title') ? event.title : '',
        categories: has(event, 'categories')
          ? (event.categories ? event.categories.map(c => c.id) : [])
          : [],
      }}
      onSubmit={async (values, actions) => {
        ReactGA.event({
          category: 'Form',
          action: editMode ? 'EditEvent' : 'NewEvent',
          transport: 'beacon',
        });
        const eventData = editMode
          ? getEventEdits({ updates: values, event })
          : {
              body: JSON.stringify(values.body),
              time: values.time,
              title: values.title,
              categories: values.categories,
            };
        // console.log('EventForm: onSubmit eventData: ', eventData);
        if (editMode) {
          // console.log('editMode eventForm eventData: ', eventData);
          await window.api
            .putEventsById(event.id, eventData)
            .then(res => res.data)
            .then(data => {
              // console.log('EventForm: onSubmit server response: ', data);
              dispatch({
                type: 'UPDATE_EVENT',
                payload: data,
              });
              actions.setSubmitting(false);
              alerts.success(`Event ${data.title} modified`);
            })
            .catch(apiError => {
              // console.log('EventForm: onSubmit error: ', apiError);
              actions.setSubmitting(false);
              const transformedError = transformApiError(apiError);
              actions.setErrors(transformedError);
            });
          setVisible(false);
          history.goBack();
        } else {
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
          sessionStorage.removeItem('eventEditing');
          setVisible(false);
          history.goBack();
        }
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
              onChange={change => setFieldValue('time', change)}
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
            <Qeditor
              formikChange={change => {
                setFieldValue('body', change);
              }}
              onBlur={handleBlur}
              body={values.body}
              eventId={params.id}
            />
          </Form.Item>
          <Form.Item
            validateStatus={errors && errors.categories ? 'error' : ''}
            help={errors && errors.categories ? errors.categories : ''}
          >
            <Select
              combobox
              mode='multiple'
              placeholder='Safety, Model 3'
              onSelect={e => {
                if (/^parent-/.test(e.toString())) {
                  openNewCategoryModal({ history, option: e, location });
                }
              }}
              value={compact([
                ...values.categories,
                sessionStorage.getItem('newCategoryChoice')
                  ? valuePerOptionName[
                      sessionStorage.getItem('newCategoryChoice')
                    ]
                  : null,
              ])}
              filterOption={(i, o) => {
                return isArray(o.props.children)
                  ? false
                  : o.props.children.toLowerCase().indexOf(i.toLowerCase()) >=
                      0;
              }}
              onChange={e => {
                setFieldValue(
                  'categories',
                  e.filter(o => !/^parent-/.test(o.toString())),
                );
                sessionStorage.removeItem('newCategoryChoice');
              }}
              onBlur={handleBlur}
              name='categories'
              size='small'
            >
              {[optionAddNewCategory, ...children]}
            </Select>
          </Form.Item>
          <Form.Item>
            <Button size='small' type='primary' htmlType='submit'>
              {editMode ? `Edit` : `Create`}
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
//EventForm.whyDidYouRender = true;

export default EventForm;
