import React, { useEffect, useContext } from 'react';
import { ModalContext } from '../contexts/ModalContext';
import { Modal } from 'antd';

const NewEventForm = props => {
    console.log('Props: ', props);
    const { visible, setVisible } = useContext(ModalContext);
    const { history, location } = props;

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
    </Modal>
  );
};

export default NewEventForm;
