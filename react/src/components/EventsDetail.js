import React, { useEffect, useContext } from 'react';
import { ModalContext } from '../contexts/ModalContext';
import { includes, isNil } from 'lodash';
import * as QueryString from 'query-string';
import { Modal } from 'antd';
import { decryptIds } from './utils/Chart';

const EventsDetail = props => {
  const { history, location, events = [] } = props;
  const { visible, setVisible } = useContext(ModalContext);
  const params = QueryString.parse(location.search);
  const eventIds = !isNil(params.id) ? decryptIds({ ids: params.id }) : [];
  useEffect(() => {
    setVisible(location.state.visible);
  }, [location]);
  const handleClose = () => {
    setVisible(false);
    history.push('/');
  };
  const eventsToDisplay = !isNil(eventIds)
    ? events.filter(e => includes(eventIds, e.id))
    : events;
  const eventDisplays = eventsToDisplay.map(e => JSON.stringify(e));

  return (
    <Modal
      title='Events'
      visible={visible}
      onOk={handleClose}
      onCancel={handleClose}
    >
      {eventDisplays}
    </Modal>
  );
};

export default EventsDetail;
