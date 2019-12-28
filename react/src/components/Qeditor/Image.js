/* eslint-disable no-unused-vars */
import React, { useState } from 'react';
import { Formik } from 'formik';
import { Transforms } from 'slate';
import { Modal, Form, Icon, Input, Button, Spin, Upload } from 'antd';
import { isUrl } from './Utils';
import imageExtensions from 'image-extensions';
import * as Yup from 'yup';

const ImageUploadSchema = Yup.object().shape({
  url: Yup.string()
    .url()
    .nullable()
    .notRequired(),
});

export const withImages = editor => {
  const { insertData, isVoid } = editor;

  editor.isVoid = element => {
    return element.type === 'image' ? true : isVoid(element);
  };

  editor.insertData = data => {
    const text = data.getData('text/plain');
    const { files } = data;

    if (files && files.length > 0) {
      for (const file of files) {
        const reader = new FileReader();
        const [mime] = file.type.split('/');

        if (mime === 'image') {
          reader.addEventListener('load', () => {
            const url = reader.result;
            insertImage(editor, url);
          });

          reader.readAsDataURL(file);
        }
      }
    } else if (isImageUrl(text)) {
      insertImage(editor, text);
    } else {
      insertData(data);
    }
  };

  return editor;
};

const { Dragger } = Upload;

const ImageUploadForm = ({ handleSubmit, changeFx, insertImage, editor }) => {
  return (
    <Formik
      initialValues={{ url: '', upload: [] }}
      onSubmit={async (values, actions) => {
        console.log(values);
        console.log(actions);
        handleSubmit(values);
      }}
      validateOnBlur={true}
      validateOnChange={false}
      validationSchema={ImageUploadSchema}
      render={({ values, errors, handleBlur, handleChange, isSubmitting }) => (
        <Form layout='vertical' onSubmit={handleSubmit}>
          <Form.Item
            label='Write or paste an image URL'
            validateStatus={errors && errors.url ? 'error' : ''}
            help={errors && errors.url ? errors.url : ''}
          >
            <Input
              prefix={<Icon type='link' style={{ color: 'rgba(0,0,0,.25)' }} />}
              type='url'
              onChange={handleChange}
              onBlur={handleBlur}
              value={values.url}
              name='url'
              size='small'
            />
          </Form.Item>
          <Form.Item
            label='Or click/drag below to select image(s)'
            validateStatus={errors && errors.upload ? 'error' : ''}
            help={errors && errors.upload ? errors.upload : ''}
          >
            <Dragger
              prefix={
                <Icon type='image-file' style={{ color: 'rgba(0,0,0,.25)' }} />
              }
              type='file'
              onChange={changeFx}
              onBlur={handleBlur}
              fileList={values.upload}
              beforeUpload={false}
              name='upload'
              size='small'
              multiple={true}
            />
          </Form.Item>
          <Form.Item>
            <Button size='small' type='primary' htmlType='submit'>
              Submit
            </Button>
          </Form.Item>
          {isSubmitting && (
            <Form.Item>
              <Spin />
            </Form.Item>
          )}
        </Form>
      )}
    />
  );
};

export const ImageButton = ({ editor }) => {
  const [visible, setVisible] = useState(false);

  const handleOK = e => {
    console.log('handleOK e: ', e);
    setVisible(false);
  };
  const handleCancel = e => {
    console.log('handleCancel e: ', e);
    setVisible(false);
  };

  const handleSubmit = e => {
    e.preventDefault();
    console.log('In submit e: ', e);
  };

  const handleChange = e => {
    // e.preventDefault();
    console.log('In change e: ', e);
  };
  return (
    <>
      <Button
        size='small'
        icon='file-image'
        onMouseDown={e => {
          e.preventDefault();
          setVisible(true);
          // const url = window.prompt('Enter the URL of the image:')
          // if (!url) return
          // insertImage(editor, url)
        }}
      />
      <Modal
        title='Image Upload'
        visible={visible}
        onOk={handleOK}
        onCancel={handleCancel}
      >
        <ImageUploadForm
          handleSubmit={handleSubmit}
          changeFx={handleChange}
          insertImage={insertImage}
          editor={editor}
        />
      </Modal>
    </>
  );
};

const insertImage = (editor, url) => {
  const text = { text: '' };
  const image = { type: 'image', url, children: [text] };
  Transforms.insertNodes(editor, image);
};

const isImageUrl = url => {
  if (!url) return false;
  if (!isUrl(url)) return false;
  const ext = new URL(url).pathname.split('.').pop();
  return imageExtensions.includes(ext);
};
