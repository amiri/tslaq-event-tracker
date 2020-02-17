import React from 'react';
import * as Yup from 'yup';
import * as alerts from '../alerts';
import { Formik } from 'formik';
import { Input, Form, Button } from 'antd';

const categoryExists = async value => {
  const res = await window.api.getCategoriesNameByName(value);
  return res.data.exists;
};

const CategorySchema = Yup.object().shape({
  parentId: Yup.string(),
  name: Yup.string()
    .required('You must provide a name')
    .test('unique', 'There is already a category by that name', async value => {
      const e = await categoryExists(value);
      return !e;
    }),
});

const transformApiError = ({ data }) => {
  if (data.title === 'CategoryConflict') {
    return { categories: data.detail };
  }
};

const NewCategoryForm = ({ setVisible, parentId, dispatch, history }) => {
  return (
    <Formik
      initialValues={{
        parentId,
        name: '',
      }}
      onSubmit={async (values, actions) => {
        await window.api
          .postCategories(values)
          .then(res => res.data)
          .then(data => {
            dispatch({
              type: 'POST_CATEGORIES',
              payload: data,
            });
            actions.setSubmitting(false);
            alerts.success(`Category ${values.name} created`);
          })
          .catch(apiError => {
            actions.setSubmitting(false);
            const transformedError = transformApiError(apiError);
            actions.setErrors(transformedError);
          });
        sessionStorage.setItem('newCategoryChoice', values.name);
        setVisible(false);
        history.goBack();
      }}
      validateOnBlur={false}
      validateOnChange={false}
      validationSchema={CategorySchema}
    >
      {({ values, errors, handleBlur, handleChange, handleSubmit }) => (
        <Form onSubmit={handleSubmit}>
          <Form.Item
            validateStatus={errors && errors.name ? 'error' : ''}
            help={errors && errors.name ? errors.name : ''}
          >
            <Input
              type='text'
              size='small'
              onChange={handleChange}
              onBlur={handleBlur}
              value={values.name}
              name='name'
            />
          </Form.Item>
          <Form.Item>
            <Button size='small' type='primary' htmlType='submit'>
              Create
            </Button>
          </Form.Item>
        </Form>
      )}
    </Formik>
  );
};

export default NewCategoryForm;
