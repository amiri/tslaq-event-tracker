import React, { useEffect, useContext } from 'react';
import { NewCategoryModalContext } from '../contexts/NewCategoryModalContext';
import { ChartContext } from '../contexts/ChartContext';
import { Modal } from 'antd';

const NewCategory = props => {
  const { visible, setVisible } = useContext(NewCategoryModalContext);
  const { allCategories } = useContext(ChartContext);
  const { history, location } = props;
  console.log(allCategories);

  useEffect(() => {
    setVisible(location.state.visible);
  }, []);

  const handleClose = () => {
    setVisible(false);
    history.goBack();
  };

  return (
    <Modal
      title='New Category'
      destroyOnClose={true}
      visible={visible}
      onCancel={handleClose}
      footer={false}
    >
      <p>New Category Form</p>
    </Modal>
  );
};

export default NewCategory;
