import React, { useCallback, useContext } from 'react';
import { AuthContext } from '../contexts/AuthContext';
import { ChartContext } from '../contexts/ChartContext';
import { EventsContext } from '../contexts/EventsContext';
import { Row, Col, Button } from 'antd';
import { BrowserRouter as Router } from 'react-router-dom';
import moment from 'moment';
import { mapValues } from 'lodash';
import {
  encryptIds,
  updateQueryParams,
  openLoginModal,
  openRegisterModal,
  openViewModal,
  openImportModal,
  convertJsonToCsv,
} from './utils/Chart';
import NavBarForm from './NavBarForm';
import ReactGA from 'react-ga';
const fileDownload = require('js-file-download');

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

const importEvents = ({ history }) => {
  openImportModal({ history });
};

const NavBar = props => {
  const { user, dispatch } = useContext(AuthContext);
  const { history, location } = props;
  const { config, setConfig, categoryOptions, valuePerOptionName } = useContext(
    ChartContext,
  );
  const { filteredEvents } = useContext(EventsContext);

  const updateSearchSubcategories = answer => {
    setConfig({ ...config, searchSubcategories: answer });
    const params = { searchSubcategories: answer };
    updateQueryParams({ params, history, location });
  };

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

  const exportEvents = () => {
    const csv = convertJsonToCsv(filteredEvents);
    const cats = config.categories.join('+');
    const dates = `${config.dateRange.startDate.format(
      'YYYY-MM-DD',
    )}-${config.dateRange.endDate.format('YYYY-MM-DD')}`;
    const fileName = `tslaq-event-tracker-${cats}-${dates}-${
      config.resolution
    }-${config.searchCondition}-${
      config.searchSubcategories
    }-${Date.now()}.tsv`;
    fileDownload(csv, fileName);
  };

  // const viewEvents = ({ history }) => {
  //   const id = encryptIds({ ids: filteredEvents.map(e => e.id) });
  //   openViewModal({ id, history });
  // };
  const viewEvents = useCallback(
    ({ history }) => {
      const id = encryptIds({ ids: filteredEvents.map(e => e.id) });
      openViewModal({ id, history });
    },
    [filteredEvents],
  );

  return (
    <Router>
      <div>
        <Row type='flex' justify='start'>
          <Col span={1} style={colStyle}>
            <div className='logo'></div>
          </Col>

          <Col span={3} style={colStyle}>
            {user ? (
              <Button
                size='small'
                type='link'
                onClick={() => logout(dispatch, history)}
              >
                Log Out
              </Button>
            ) : (
              <>
                <Button
                  size='small'
                  type='link'
                  onClick={() => openLoginModal({ history })}
                >
                  Log In
                </Button>
                <Button
                  size='small'
                  type='link'
                  onClick={() => {
                    ReactGA.event({
                      category: 'Modal',
                      action: 'OpenRegister',
                      transport: 'beacon',
                    });
                    openRegisterModal({ history });
                  }}
                >
                  Register
                </Button>
              </>
            )}
          </Col>

          <Col span={18} style={colStyle}>
            <NavBarForm
              valuePerOptionName={valuePerOptionName}
              config={config}
              categoryOptions={categoryOptions}
              history={history}
              location={location}
              updateCategories={updateCategories}
              updateRange={updateRange}
              updateSearchCondition={updateSearchCondition}
              updateSearchSubcategories={updateSearchSubcategories}
              viewEvents={viewEvents}
            />
          </Col>
          <Col span={2} style={colStyle}>
            <>
              {filteredEvents.length > 0 ? (
                <Button
                  size='small'
                  type='link'
                  onClick={() => {
                    ReactGA.event({
                      category: 'Modal',
                      action: 'OpenExport',
                      transport: 'beacon',
                    });
                    exportEvents();
                  }}
                >
                  Export
                </Button>
              ) : null}
              {user ? (
                <Button
                  size='small'
                  type='link'
                  onClick={() => {
                    ReactGA.event({
                      category: 'Modal',
                      action: 'OpenImport',
                      transport: 'beacon',
                    });
                    importEvents({ history });
                  }}
                >
                  Import
                </Button>
              ) : null}
            </>
          </Col>
        </Row>
      </div>
    </Router>
  );
};
//NavBar.whyDidYouRender = true;

export default NavBar;
