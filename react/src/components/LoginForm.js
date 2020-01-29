import React, * as react from 'react';
import { Form, Icon, Input, Button, Spin } from 'antd';
import { Formik } from 'formik';
import * as Yup from 'yup';
import { AuthContext } from '../contexts/AuthContext';
import * as alerts from '../alerts';
import { Link } from 'react-router-dom';

const SignupSchema = Yup.object().shape({
  email: Yup.string()
    .email('Your email address is invalid')
    .required('You must enter your email address'),
  password: Yup.string().required('You must enter your password'),
});

const transformApiError = () => {
  return {
    email: 'Please check your email.',
    password: 'Please check your password.',
  };
};

const LoginForm = () => {
  const { dispatch } = react.useContext(AuthContext);
  return (
    <Formik
      initialValues={{ email: '', password: '' }}
      onSubmit={async (values, actions) => {
        const loginData = {
          emailAddress: values.email,
          password: values.password,
        };
        await window.api
          .postLogin(loginData)
          .then(res => res.data)
          .then(u => {
            dispatch({
              type: 'LOGIN_SUCCESS',
              payload: u,
            });
            actions.setSubmitting(false);
            alerts.success(`Welcome, ${u.authUserName}.`);
          })
          .catch(apiError => {
            sessionStorage.removeItem('user');
            console.error('Login error: ', apiError);
            dispatch({
              type: 'LOGIN_FAILURE',
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
      validationSchema={SignupSchema}
      render={({
        values,
        errors,
        handleBlur,
        handleChange,
        handleSubmit,
        isSubmitting,
      }) => (
        <Form layout='inline' onSubmit={handleSubmit}>
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
              Log in
            </Button>
          </Form.Item>
          <Form.Item>
            <Link to={{ pathname: '/register' }}>Register</Link>
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

export default LoginForm;
