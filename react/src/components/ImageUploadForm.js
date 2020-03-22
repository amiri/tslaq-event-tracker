import axios from 'axios';
import React, { useContext } from 'react';
import { AuthContext } from '../contexts/AuthContext';
import { Formik } from 'formik';
import { Form, Icon, Input, Button, Spin, Upload } from 'antd';
import * as Yup from 'yup';
import { isNil, compact } from 'lodash';
import ReactGA from 'react-ga';
import { isImageUrl } from './Qeditor/Image';
import { useHistory } from 'react-router';

const ImageUploadSchema = Yup.object().shape({
  url: Yup.string()
    .nullable()
    .notRequired()
    .test('isImageUrl', 'This does not appear to be an image URL', v => {
      return isNil(v) || isImageUrl(v);
    }),
});
const { Dragger } = Upload;

const ImageUploadForm = ({ setVisible }) => {
  const { user } = useContext(AuthContext);
  const history = useHistory();

  return (
    <Formik
      initialValues={{ url: null, upload: [] }}
      validateOnBlur={true}
      validateOnChange={true}
      validationSchema={ImageUploadSchema}
      /* eslint-disable-next-line no-unused-vars */
      onSubmit={async (values, actions) => {
        ReactGA.event({
          category: 'Qeditor',
          action: 'ImageUpload',
          transport: 'beacon',
        });
        // console.log('about to await urls');
        const urls = await Promise.all(
          values.upload.map(async f => {
            const date = Date.now();
            const fileName = `${user.authUserName}-${date}-${f.name}`;
            const fileInfo = {
              name: fileName,
              contentType: f.type,
            };
            // console.log('about to postSign for ', fileInfo);
            await window.api
              .postSign(fileInfo)
              .then(res => res.data)
              .then(async data => {
                // console.log('about to put file to s3 ', data);
                await axios({
                  url: data.url,
                  method: 'put',
                  data: f,
                  headers: { 'Content-Type': f.type },
                  withCredentials: true,
                })
                  .then(res => {
                    console.log('put file successfully: ', res);
                  })
                  .catch(err => {
                    console.error('s3 put: error: ', err);
                  });
              })
              .catch(apiError => {
                console.error('postSign: error: ', apiError);
              });
            return 'https://images.tslaq-event-tracker.org/' + fileName;
          }),
        );
        const allUrls = compact([values.url, ...urls]);
        sessionStorage.setItem('imageUploads', JSON.stringify(allUrls));
        setVisible(false);
        history.goBack();
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
//ImageUploadForm.whyDidYouRender = true;

export default ImageUploadForm;
