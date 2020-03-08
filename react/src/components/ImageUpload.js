import React, { useContext, useEffect } from 'react';
import { ImageModalContext } from '../contexts/ImageModalContext';
import { Modal } from 'antd';
import ImageUploadForm from './ImageUploadForm';

const ImageUpload = props => {
  const { visible, setVisible } = useContext(ImageModalContext);
  const { history, location } = props;

  useEffect(() => {
    setVisible(location.state.visible);
  }, []);

  const handleClose = e => {
    e.preventDefault();
    setVisible(false);
    history.goBack();
  };

  return (
    <Modal
      title='Image Upload'
      destroyOnClose={true}
      visible={visible}
      onOk={handleClose}
      onCancel={handleClose}
      footer={false}
    >
      <ImageUploadForm setVisible={setVisible} />
    </Modal>
  );
};
ImageUpload.whyDidYouRender = true;
export default ImageUpload;
