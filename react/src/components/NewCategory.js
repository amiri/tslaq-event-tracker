import React, { useEffect, useContext } from 'react';
import { NewCategoryModalContext } from '../contexts/NewCategoryModalContext';
import { ChartContext } from '../contexts/ChartContext';
import { Modal } from 'antd';
import NewCategoryForm from './NewCategoryForm';

const NewCategory = props => {
  const { visible, setVisible } = useContext(NewCategoryModalContext);
  const { valuePerOptionName, dispatch, fullNamePerOptionValue } = useContext(
    ChartContext,
  );
  const { history, location } = props;
  const option = location.state.option;

  useEffect(() => {
    setVisible(location.state.visible);
  }, []);

  const handleClose = () => {
    setVisible(false);
    history.goBack();
  };

  const [_, parentId] = option.split('-');
  const subName = fullNamePerOptionValue[parentId];
  const title = parentId
    ? `New ${subName} Subcategory`
    : 'New Top-Level Category';

  return (
    <Modal
      title={`${title}`}
      destroyOnClose={true}
      visible={visible}
      onCancel={handleClose}
      footer={false}
    >
      <NewCategoryForm
        valuePerOptionName={valuePerOptionName}
        setVisible={setVisible}
        parentId={parentId}
        dispatch={dispatch}
        history={history}
        location={location}
      />
    </Modal>
  );
};

export default NewCategory;
