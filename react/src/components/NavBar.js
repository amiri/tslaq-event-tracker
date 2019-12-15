import React, { useContext } from 'react';
import { AuthContext } from '../contexts/AuthContext';
import { ChartContext } from '../contexts/ChartContext';
import LoginForm from './LoginForm';
import RegisterForm from './RegisterForm';
import { Row, Col, Button, Typography } from 'antd';
import { BrowserRouter as Router, Switch, Route } from 'react-router-dom';
import { DatePicker, Select } from 'antd';
import moment from 'moment';
import { isEmpty } from 'lodash';

const { RangePicker } = DatePicker;
const { Text } = Typography;

const logout = async dispatch => {
  localStorage.removeItem('user');
  await window.api
    .getLogout()
    .then(d => dispatch({ type: 'LOGOUT', payload: d }));
};

const colStyle = {
  display: 'flex',
  justifyContent: 'flex-start',
  alignItems: 'center',
};

const NavBar = () => {
  const { user, dispatch } = useContext(AuthContext);
  const { config, setConfig, categoryOptions } = useContext(ChartContext);
    const  {Option} = Select;
    const options = categoryOptions.map(o => <Option key={o.id}>{o.name}</Option>);
    console.log(options);
  const updateCategories = categories => {
    setConfig({...config, categories});
  };
  const updateRange = dates => {
    const estDates = dates.map(d =>
      moment.tz(d.format('YYYY-MM-DD 00:00:00'), config.timeZone),
    );
    setConfig({ ...config, dateRange: estDates });
  };
  return (
    <Router>
      <div>
        <Row type='flex' justify='start'>
          <Col style={colStyle}>
            <Text strong={true} style={{ marginRight: '1em' }}>
              $TSLAQ Event Tracker
            </Text>
          </Col>
          <Col style={colStyle}>
            {user ? (
              <Button type='link' onClick={() => logout(dispatch)}>
                Logout
              </Button>
            ) : (
              <Switch>
                <Route exact path='/' component={LoginForm} />
                <Route path='/login' component={LoginForm} />
                <Route path='/register' component={RegisterForm} />
              </Switch>
            )}
          </Col>
          <Col style={{ ...colStyle, marginLeft: 'auto', order: 2 }}>
            <Text strong={true} style={{ marginRight: '1em' }}>
              Categories:
            </Text>
            <Select style={{width: '100%'}}allowClear={true} mode='multiple' placeholder='Safety, Model 3' onChange={values => updateCategories(values)} >{options}</Select>
          </Col>
          <Col style={{ ...colStyle, marginLeft: 'auto', order: 3 }}>
            <Text strong={true} style={{ marginRight: '1em' }}>
              Date Range:
            </Text>
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
