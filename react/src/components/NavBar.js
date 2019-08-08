import React, { useContext } from 'react';
import { AuthContext } from '../contexts/AuthContext';
import LoginForm from './LoginForm';
import { Row, Col, Button } from 'antd';
import { BrowserRouter as Router } from 'react-router-dom';
import { DatePicker } from 'antd';

const { RangePicker } = DatePicker;

const logout = async dispatch => {
  localStorage.removeItem('user');
  await window.api
    .getLogout()
    .then(d => dispatch({ type: 'LOGOUT', payload: d }));
};

const colStyle = {
  display: 'flex',
  justifyContent: 'center',
  alignItems: 'center',
};

const NavBar = () => {
  const { user, dispatch } = useContext(AuthContext);
  return (
    <Router>
      <div>
        <Row type='flex' justify='start'>
          <Col style={colStyle}>Event Tracker</Col>
          <Col style={colStyle}>
            {user ? (
              <Button type='link' onClick={() => logout(dispatch)}>
                Logout
              </Button>
            ) : (
              <LoginForm />
            )}
          </Col>
          <Col style={colStyle}>
            Chart daterange: <RangePicker />
          </Col>
        </Row>
      </div>
    </Router>
  );
};

export default NavBar;
