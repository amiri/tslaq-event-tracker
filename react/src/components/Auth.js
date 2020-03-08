import React, { useEffect, useContext } from 'react';
import { Route, Redirect, useLocation } from 'react-router-dom';
import { AuthModalContext } from '../contexts/AuthModalContext';
import { AuthContext } from '../contexts/AuthContext';
import LoginForm from './LoginForm';
import RegisterForm from './RegisterForm';
import { Modal } from 'antd';

const Auth = props => {
  const { visible, setVisible } = useContext(AuthModalContext);
  const { history, location } = props;
  const destination = location.state.from ? location.state.from : '/';

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
        <LoginForm
          setVisible={setVisible}
          destination={destination}
          history={history}
        />
      ) : (
        <RegisterForm
          setVisible={setVisible}
          destination={destination}
          history={history}
        />
      )}
    </Modal>
  );
};

export const UserRequired = ({
  component: Component = null,
  render: Render = null,
  ...rest
}) => {
  const location = useLocation();
  const { user } = useContext(AuthContext);
  return (
    <Route
      {...rest}
      render={props =>
        user ? (
          Render ? (
            Render(props)
          ) : Component ? (
            <Component {...props} />
          ) : null
        ) : (
          <Redirect
            to={{
              pathname: '/login',
              state: { from: location, visible: true },
            }}
          />
        )
      }
    />
  );
};
Auth.whyDidYouRender = true;

export default Auth;
