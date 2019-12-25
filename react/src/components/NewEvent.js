import React, { useEffect, useContext } from 'react';
import { ModalContext } from '../contexts/ModalContext';
import { ChartContext } from '../contexts/ChartContext';
import EventForm from './EventForm';
import { Modal, Select } from 'antd';

const { Option } = Select;

const NewEvent = props => {
  const { visible, setVisible } = useContext(ModalContext);
  const { allCategories } = useContext(ChartContext);
  const categoryOptions = allCategories.map(o => (
    <Option key={o.id} value={o.id} label={o.name}>
      {o.name}
    </Option>
  ));
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
      visible={visible}
      onOk={handleClose}
      onCancel={handleClose}
    >
      <EventForm
        categoryOptions={categoryOptions}
        event={{ time: eventDate }}
      />
    </Modal>
  );
};

export default NewEvent;
