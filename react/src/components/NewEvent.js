import React, { useEffect, useContext } from 'react';
import { ModalContext } from '../contexts/ModalContext';
import { ChartContext } from '../contexts/ChartContext';
import EventForm from './EventForm';
import { Modal } from 'antd';

const NewEvent = props => {
  const { visible, setVisible } = useContext(ModalContext);
  const { categoryOptions } = useContext(ChartContext);
  const { history, location } = props;
  const eventDate = location.state.eventDate;

  useEffect(() => {
    setVisible(location.state.visible);
  }, [location]);

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
        event={{ time: eventDate }}
        setVisible={setVisible}
      />
    </Modal>
  );
};

export default NewEvent;
