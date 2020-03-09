import React, { useEffect, useContext } from 'react';
import { ImportModalContext } from '../contexts/ImportModalContext';
import { ChartContext } from '../contexts/ChartContext';
import { EventsContext } from '../contexts/EventsContext';
import { Modal } from 'antd';
import ImportForm from './ImportForm';

const Import = props => {
  const { visible, setVisible } = useContext(ImportModalContext);
  const { valuePerOptionFullName } = useContext(ChartContext);
  const { dispatch } = useContext(EventsContext);
  const { history, location } = props;

  useEffect(() => {
    setVisible(location.state.visible);
  }, []);

  const handleClose = () => {
    setVisible(false);
    history.goBack();
  };

  return (
    <Modal
      title='Import New Events'
      destroyOnClose={true}
      visible={visible}
      onCancel={handleClose}
      footer={false}
    >
      <ImportForm
        dispatch={dispatch}
        valuePerOptionFullName={valuePerOptionFullName}
        setVisible={setVisible}
      />
    </Modal>
  );
};
//Import.whyDidYouRender = true;

export default Import;
