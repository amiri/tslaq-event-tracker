import React, { useContext } from 'react';
import { AuthContext } from '../contexts/AuthContext';
import { ChartContext } from '../contexts/ChartContext';
import { EventsContext } from '../contexts/EventsContext';
import LoginForm from './LoginForm';
import RegisterForm from './RegisterForm';
import { Row, Col, Button, Typography } from 'antd';
import { BrowserRouter as Router, Switch, Route } from 'react-router-dom';
import { DatePicker, Select, Radio } from 'antd';
import moment from 'moment';
import { isEmpty, mapValues } from 'lodash';
import EventsDetail from './EventsDetail';
import { encryptIds, updateQueryParams } from './utils/Chart';

const { RangePicker } = DatePicker;
const { Text } = Typography;

const logout = async (dispatch, history) => {
  sessionStorage.removeItem('user');
  await window.api.getLogout().then(d => {
    dispatch({ type: 'LOGOUT', payload: d });
    history.push('/');
  });
};

const colStyle = {
  display: 'flex',
  justifyContent: 'flex-start',
  alignItems: 'center',
};

const radioStyle = {
  display: 'block',
};

const { Option } = Select;

const NavBar = props => {
  const { user, dispatch } = useContext(AuthContext);
  const { history, location } = props;
  const { config, setConfig, allCategories } = useContext(ChartContext);
  const { filteredEvents } = useContext(EventsContext);
  const options = allCategories.map(o => <Option key={o.id}>{o.name}</Option>);

  const updateCategories = categories => {
    setConfig({ ...config, categories });
    updateQueryParams({ params: { categories }, history, location });
  };

  const updateRange = dates => {
    const estDates = dates.map(d =>
      moment.tz(d.format('YYYY-MM-DD 00:00:00'), config.timeZone),
    );
    const dateRange = { startDate: estDates[0], endDate: estDates[1] };
    setConfig({
      ...config,
      dateRange,
    });
    const params = mapValues(dateRange, v => {
      return v.format('YYYY-MM-DD');
    });
    updateQueryParams({ params, history, location });
  };

  const updateSearchCondition = e => {
    setConfig({ ...config, searchCondition: e.target.value });
    const params = { searchCondition: e.target.value };
    updateQueryParams({ params, history, location });
  };

  const viewEvents = () => {
    const id = encryptIds({ ids: filteredEvents.map(e => e.id) });
    history.push({
      pathname: '/event/',
      search: `?id=${id}`,
      state: { visible: true },
    });
  };

  return (
    <Router>
      <Route path='/event' render={props => <EventsDetail {...props} />} />
      <div>
        <Row type='flex' justify='start'>
          <Col style={colStyle}>
            <div className='logo'></div>
          </Col>
          <Col style={colStyle}>
            {user ? (
              <Button
                size='small'
                type='link'
                onClick={() => logout(dispatch, history)}
              >
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
          <Col span={4} style={{ ...colStyle, marginLeft: 'auto', order: 2 }}>
            <Text strong={true} style={{ marginRight: '1em' }}>
              Categories:
            </Text>
            <Select
              style={{ width: '100%', marginRight: '1em' }}
              allowClear={true}
              mode='multiple'
              placeholder='Safety, Model 3'
              onChange={values => updateCategories(values)}
            >
              {options}
            </Select>
          </Col>
          <Col style={{ ...colStyle, order: 3 }}>
            <Radio.Group
              size='small'
              onChange={updateSearchCondition}
              value={config.searchCondition}
            >
              <Radio size='small' style={radioStyle} value='and'>
                and
              </Radio>
              <Radio size='small' style={radioStyle} value='or'>
                or
              </Radio>
            </Radio.Group>
          </Col>
          <Col style={{ ...colStyle, order: 4 }}>
            <Text strong={true} style={{ marginRight: '1em' }}>
              Date Range:
            </Text>
            <RangePicker
              size='small'
              allowClear={true}
              onChange={dates => updateRange(dates)}
              value={
                !isEmpty(config.dateRange)
                  ? [config.dateRange.startDate, config.dateRange.endDate]
                  : null
              }
            />
          </Col>
          <Col style={{ ...colStyle, marginLeft: '1em', order: 5 }}>
            <Button
              type='primary'
              size='small'
              onClick={() => viewEvents({ history })}
            >
              View events
            </Button>
          </Col>
        </Row>
      </div>
    </Router>
  );
};

export default NavBar;
