import React, { useEffect, useContext } from 'react';
import { EditEventModalContext } from '../contexts/EditEventModalContext';
import { EventsContext } from '../contexts/EventsContext';
import { ChartContext } from '../contexts/ChartContext';
import EventForm from './EventForm';
import { Modal } from 'antd';
import * as QueryString from 'query-string';
import { isNil } from 'lodash';
import { decryptIds } from './utils/Chart';

const EditEvent = props => {
  const { visible, setVisible } = useContext(EditEventModalContext);
  const { categoryOptions, valuePerOptionName } = useContext(ChartContext);
  const { history, location } = props;
  const { filteredEvents } = useContext(EventsContext);
  const params = QueryString.parse(location.search);
  const eventIds = !isNil(params.id) ? decryptIds({ ids: params.id }) : [];
  const eventId = location.state ? location.state.eventId : eventIds[0];
  const event = filteredEvents.find(e => e.id === eventId);
  if (event) {
    sessionStorage.setItem('eventEditing', JSON.stringify(event));
  }
  const imageUploads = sessionStorage.getItem('imageUploads');

  useEffect(() => {
    setVisible(location.state ? location.state.visible : true);
  }, []);

  const handleClose = () => {
    sessionStorage.removeItem('eventEditing');
    sessionStorage.removeItem('imageUploads');
    setVisible(false);
    history.goBack();
  };

  return (
    <Modal
      title='Edit Event'
      destroyOnClose={true}
      visible={visible}
      onCancel={handleClose}
      footer={false}
    >
      <EventForm
        categoryOptions={categoryOptions}
        valuePerOptionName={valuePerOptionName}
        event={event}
        setVisible={setVisible}
        history={history}
        location={location}
        imageUploads={imageUploads}
      />
    </Modal>
  );
};
//EditEvent.whyDidYouRender = true;

export default EditEvent;
