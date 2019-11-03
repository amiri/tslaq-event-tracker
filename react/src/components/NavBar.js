import React, { useContext } from 'react';
import { AuthContext } from '../contexts/AuthContext';
import { ChartContext } from '../contexts/ChartContext';
import LoginForm from './LoginForm';
import { Row, Col, Button } from 'antd';
import { BrowserRouter as Router } from 'react-router-dom';
import { DatePicker } from 'antd';
import moment from 'moment';
import { isEmpty } from 'lodash';

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
  const { config, setConfig } = useContext(ChartContext);
  const updateRange = dates => {
    const estDates = dates.map(d =>
      moment.tz(d.format('YYYY-MM-DD 00:00:00'), config.timeZone),
    );
    setConfig({ ...config, dateRange: estDates });
  };
  // console.log('DateRange in navbar: ', config.dateRange);
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
            Chart daterange:{' '}
            <RangePicker
              allowClear={true}
              onChange={dates => updateRange(dates)}
              value={!isEmpty(config.dateRange) ? config.dateRange : null}
            />
          </Col>
        </Row>
      </div>
    </Router>
  );
};

export default NavBar;
