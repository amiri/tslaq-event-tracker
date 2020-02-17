import React, { useEffect, useContext } from 'react';
import { AuthModalContext } from '../contexts/AuthModalContext';
import LoginForm from './LoginForm';
import RegisterForm from './RegisterForm';
import { Modal } from 'antd';

const Auth = props => {
  const { visible, setVisible } = useContext(AuthModalContext);
  const { history, location } = props;

  useEffect(() => {
    setVisible(location.state.visible);
  }, []);

  const handleClose = () => {
    setVisible(false);
    history.push('/');
  };

  const isLogin = /login/.test(location.pathname) ? true : false;
  return (
    <Modal
      title={isLogin ? 'Log In' : 'Register'}
      destroyOnClose={true}
      visible={visible}
      onCancel={handleClose}
      footer={false}
    >
      {isLogin ? (
        <LoginForm setVisible={setVisible} />
      ) : (
        <RegisterForm setVisible={setVisible} />
      )}
    </Modal>
  );
};

export default Auth;
