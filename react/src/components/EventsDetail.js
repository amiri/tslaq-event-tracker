import React, { useEffect, useContext } from 'react';
import { ModalContext } from '../contexts/ModalContext';
import { includes } from 'lodash';
import * as QueryString from 'query-string';
import { Modal } from 'antd';

const EventsDetail = props => {
  const { history, location, events } = props;
  const { visible, setVisible } = useContext(ModalContext);
  const params = QueryString.parse(location.search);
  // console.log('Show modal with query string: ', props);
  // console.log('Show modal with query string: ', params);
  // console.log('Show modal with query string: ', history);
  // console.log('Show modal with query string: ', location);
  useEffect(() => {
    setVisible(location.state.visible);
  }, [location]);
  const handleClose = () => {
    setVisible(false);
    history.push('/');
  };
  const eventsToDisplay = events.filter(e => includes([params.id], e.id));
  return (
    <Modal
      title='Events'
      visible={visible}
      onOk={handleClose}
      onCancel={handleClose}
    >
      <p>Here is event {JSON.stringify(eventsToDisplay)}.</p>
    </Modal>
  );
};

export default EventsDetail;
