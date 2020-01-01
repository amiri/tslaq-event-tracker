import axios from 'axios';
import React, { useState, useContext } from 'react';
import { AuthContext } from '../../contexts/AuthContext';
import { Formik } from 'formik';
import { Transforms } from 'slate';
import { Modal, Form, Icon, Input, Button, Spin, Upload } from 'antd';
import { isUrl } from './Utils';
import imageExtensions from 'image-extensions';
import * as Yup from 'yup';
import { isNil, compact } from 'lodash';

const ImageUploadSchema = Yup.object().shape({
  url: Yup.string()
    .nullable()
    .notRequired()
    .test('isImageUrl', 'This does not appear to be an image URL', v => {
      return isNil(v) || isImageUrl(v);
    }),
});

export const withImages = editor => {
  const { insertData } = editor;

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

const ImageUploadForm = ({ editor, setVisible }) => {
  const { user } = useContext(AuthContext);

  return (
    <Formik
      initialValues={{ url: null, upload: [] }}
      validateOnBlur={true}
      validateOnChange={true}
      validationSchema={ImageUploadSchema}
      /* eslint-disable-next-line no-unused-vars */
      onSubmit={async (values, actions) => {
        const urls = await Promise.all(
          values.upload.map(async f => {
            const date = Date.now();
            const fileName = `${user.authUserName}-${date}-${f.name}`;
            const fileInfo = {
              name: fileName,
              contentType: f.type,
            };
            return await window.api
              .postSign(fileInfo)
              .then(res => res.data)
              .then(async data => {
                return await axios({
                  url: data.url,
                  method: 'put',
                  data: f,
                  headers: { 'Content-Type': f.type },
                  withCredentials: true,
                })
                  .then(() => {
                    return 'https://images.tslaq-event-tracker.org/' + fileName;
                  })
                  .catch(err => {
                    console.log('Put s3 error: ', err);
                  });
              })
              .catch(apiError => {
                console.log('Sign upload error: ', apiError);
              });
          }),
        );
        const allUrls = compact([values.url, ...urls]);
        await Promise.all(allUrls.map(async u => insertImage(editor, u)));
        setVisible(false);
      }}
      render={({
        values,
        errors,
        handleBlur,
        handleChange,
        handleSubmit,
        setFieldValue,
        isSubmitting,
      }) => (
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
              accept='image/*'
              onChange={handleChange}
              onBlur={handleBlur}
              fileList={values.upload}
              onRemove={file => {
                const i = values.upload.indexOf(file);
                const n = values.upload.slice();
                n.splice(i, 1);
                setFieldValue('upload', n);
              }}
              beforeUpload={(_, files) => {
                setFieldValue('upload', [...values.upload, ...files]);
                return false;
              }}
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
    e.preventDefault();
    setVisible(false);
  };
  const handleCancel = e => {
    e.preventDefault();
    setVisible(false);
  };

  return (
    <>
      <Button
        size='small'
        icon='file-image'
        onMouseDown={e => {
          e.preventDefault();
          setVisible(true);
        }}
      />
      <Modal
        title='Image Upload'
        destroyOnClose={true}
        visible={visible}
        onOk={handleOK}
        onCancel={handleCancel}
        footer={false}
      >
        <ImageUploadForm setVisible={setVisible} editor={editor} />
      </Modal>
    </>
  );
};

const insertImage = (editor, url) => {
  const text = { text: '' };
  const image = { type: 'image', url, children: [text] };
  Transforms.insertNodes(editor, image);
  Transforms.insertNodes(editor, { type: 'paragraph', children: [text] });
};

const isImageUrl = url => {
  if (!url) return false;
  if (!isUrl(url)) return false;
  const ext = new URL(url).pathname.split('.').pop();
  return imageExtensions.includes(ext);
};
