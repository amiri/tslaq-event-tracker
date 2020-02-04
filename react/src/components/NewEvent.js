import React, { useEffect, useContext } from 'react';
import { NewEventModalContext } from '../contexts/NewEventModalContext';
import { ChartContext } from '../contexts/ChartContext';
import EventForm from './EventForm';
import { Modal } from 'antd';

const NewEvent = props => {
  const { visible, setVisible } = useContext(NewEventModalContext);
  const { categoryOptions, valuePerOptionName } = useContext(ChartContext);
  const { history, location } = props;
  const eventDate = location.state.eventDate;

  useEffect(() => {
    setVisible(location.state.visible);
  }, []);

  const handleClose = () => {
    setVisible(false);
    history.push('/');
  };

  return (
    <Modal
      title='New Event'
      destroyOnClose={true}
      visible={visible}
      onCancel={handleClose}
      footer={false}
    >
      <EventForm
        categoryOptions={categoryOptions}
        valuePerOptionName={valuePerOptionName}
        event={{ time: eventDate }}
        setVisible={setVisible}
        history={history}
      />
    </Modal>
  );
};

export default NewEvent;
