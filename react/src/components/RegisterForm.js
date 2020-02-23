import React, * as react from 'react';
import { Form, Icon, Input, Button, Spin } from 'antd';
import { Formik } from 'formik';
import * as Yup from 'yup';
import { AuthContext } from '../contexts/AuthContext';
import * as alerts from '../alerts';
import ReactGA from 'react-ga';

const RegisterSchema = Yup.object().shape({
  email: Yup.string()
    .email('Your email address is invalid')
    .required('You must enter your email address'),
  password: Yup.string().required('You must enter your password'),
  name: Yup.string().required('You must enter your username'),
});

const transformApiError = ({ statusText }) => {
  return {
    email:
      statusText === 'Conflict'
        ? 'This email may already be registered'
        : 'Please check your email.',
    name:
      statusText === 'Conflict'
        ? 'This name may already be registered'
        : 'Please check your name.',
  };
};

const RegisterForm = ({ setVisible, destination, history }) => {
  const { dispatch } = react.useContext(AuthContext);
  return (
    <Formik
      initialValues={{ email: '', password: '', name: '' }}
      onSubmit={async (values, actions) => {
        ReactGA.event({
          category: 'Form',
          action: 'Register',
          transport: 'beacon',
        });
        const registerData = {
          emailAddress: values.email,
          password: values.password,
          name: values.name,
        };
        await window.api
          .postRegister(registerData)
          .then(res => res.data)
          .then(u => {
            dispatch({
              type: 'REGISTER_SUCCESS',
              payload: u,
            });
            actions.setSubmitting(false);
            alerts.success(`Welcome, ${u.authUserName}.`);
            setVisible(false);
            history.push(destination);
          })
          .catch(apiError => {
            sessionStorage.removeItem('user');
            console.error('Register error: ', apiError);
            dispatch({
              type: 'REGISTER_FAILURE',
              payload: apiError,
            });
            actions.setSubmitting(false);
            actions.setErrors(transformApiError(apiError));
            apiError &&
              alerts.error(`${apiError.statusText}: ${apiError.data.detail}`);
          });
      }}
      validateOnBlur={false}
      validateOnChange={false}
      validationSchema={RegisterSchema}
      render={({
        values,
        errors,
        handleBlur,
        handleChange,
        handleSubmit,
        isSubmitting,
      }) => (
        <Form layout='vertical' onSubmit={handleSubmit}>
          <Form.Item
            validateStatus={errors && errors.email ? 'error' : ''}
            help={errors && errors.email ? errors.email : ''}
          >
            <Input
              prefix={<Icon type='mail' style={{ color: 'rgba(0,0,0,.25)' }} />}
              placeholder='me@me.com'
              type='email'
              onChange={handleChange}
              onBlur={handleBlur}
              value={values.email}
              name='email'
              size='small'
            />
          </Form.Item>
          <Form.Item
            validateStatus={errors && errors.name ? 'error' : ''}
            help={errors && errors.name ? errors.name : ''}
          >
            <Input
              prefix={<Icon type='user' style={{ color: 'rgba(0,0,0,.25)' }} />}
              type='text'
              placeholder='joegreen'
              onChange={handleChange}
              onBlur={handleBlur}
              value={values.name}
              name='name'
              size='small'
            />
          </Form.Item>
          <Form.Item
            validateStatus={errors && errors.password ? 'error' : ''}
            help={errors && errors.password ? errors.password : ''}
          >
            <Input
              prefix={<Icon type='lock' style={{ color: 'rgba(0,0,0,.25)' }} />}
              type='password'
              placeholder='abc123'
              onChange={handleChange}
              onBlur={handleBlur}
              value={values.password}
              name='password'
              size='small'
            />
          </Form.Item>
          <Form.Item>
            <Button size='small' type='primary' htmlType='submit'>
              Register
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

export default RegisterForm;
