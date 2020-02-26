import React from 'react';
import moment from 'moment';
require('moment-timezone');
import { Formik } from 'formik';
import { Table, Typography, Form, Icon, Button, Spin, Upload } from 'antd';
import * as Yup from 'yup';
import ReactGA from 'react-ga';
import { readString } from 'react-papaparse';
import * as alerts from '../alerts';

const transformApiError = error => {
  if (error.data.title === 'EventConflict') {
    const msg = `${error.data.detail}. (${error.config.data})`;
    return { upload: msg };
  }
};

const { Text } = Typography;

const dataSource = [
  {
    key: '1',
    body: 'The solar-roof reveal sealed the Solar City deal in 2016.',
    time: moment('Fri 28 Oct 2016 18:43 PDT').format(),
    title: 'Solar City Synergies',
    categories: 'Fraud > Conveyance',
  },
  {
    key: '2',
    body: 'Elon Musk announces a take-private bid',
    time: moment('2018-08-07 09:48')
      .tz('America/Los_Angeles')
      .format(),
    title: 'Funding Secured',
    categories: 'Fraud > Securities',
  },
];

const columns = [
  {
    title: 'time',
    dataIndex: 'time',
    key: 'time',
  },
  {
    title: 'title',
    dataIndex: 'title',
    key: 'title',
  },
  {
    title: 'body',
    dataIndex: 'body',
    key: 'body',
  },
  {
    title: 'categories',
    dataIndex: 'categories',
    key: 'categories',
  },
];

const parseFile = ({ contents, config }) => {
  const results = readString(contents, config);
  return results;
};

const convertToSlateJson = body => {
  return [{ type: 'paragraph', children: [{ text: body }] }];
};

const ImportForm = ({ dispatch, setVisible, valuePerOptionFullName }) => {
  const EventSchema = Yup.object().shape({
    body: Yup.array()
      .of(Yup.object())
      .min(1)
      .required('You must enter the text of the event.'),
    time: Yup.date().required('You must enter the event date and time.'),
    title: Yup.string().required('You must enter the title of the event.'),
    categories: Yup.string()
      .required('You must choose at least one category.')
      .test(
        'categoryExists',
        'There is no category with that full name.',
        value => {
          return valuePerOptionFullName[value] ? true : false;
        },
      ),
  });

  const UploadSchema = Yup.object().shape({
    upload: Yup.mixed().required('You must choose a file to upload.'),
  });

  return (
    <Formik
      initialValues={{
        upload: '',
      }}
      onSubmit={async (values, actions) => {
        ReactGA.event({
          category: 'Form',
          action: 'ImportEvents',
          transport: 'beacon',
        });
        const file = values.upload[0];
        const reader = new FileReader();
        const [_, mimeType] = file.type.split('/');
        var imports = [];
        var validationErrors = [];
        const parseConfig = {
          delimiter: '',
          newline: '',
          quoteChar: '"',
          escapeChar: '"',
          header: true,
          transformHeader: undefined,
          dynamicTyping: false,
          preview: 0,
          encoding: 'utf8',
          worker: false,
          comments: false,
          step: (results, parser) => {
            try {
              const res = EventSchema.validateSync(results.data);
              imports.push(res);
            } catch (error) {
              parser.abort();
              const badRow = JSON.stringify(error.value);
              actions.setSubmitting(false);
              const errorToSet = { upload: `${error.message}: ${badRow}` };
              validationErrors.push(errorToSet);
            }
          },
          complete: undefined,
          error: null,
          download: false,
          downloadRequestHeaders: undefined,
          skipEmptyLines: true,
          chunk: undefined,
          fastMode: undefined,
          beforeFirstChunk: undefined,
          withCredentials: undefined,
          transform: function(value, header) {
            if (header === 'time') {
              return moment(value).tz('America/New_York');
            } else if (header === 'body') {
              return convertToSlateJson(value);
            } else {
              return value;
            }
          },
        };
        if (mimeType === 'csv') {
          reader.addEventListener('load', async () => {
            const lines = reader.result.split(/[\r\n]+/g);
            const headers = lines[0].split(/,/g);
            if (
              headers.filter(h => h === 'time').length === 0 ||
              headers.filter(h => h === 'title').length === 0 ||
              headers.filter(h => h === 'body').length === 0 ||
              headers.filter(h => h === 'categories').length === 0
            ) {
              validationErrors.push({
                upload:
                  'You must include a complete and valid header row in your *.csv.',
              });
            }
            parseFile({ contents: reader.result, config: parseConfig });
            if (validationErrors.length === 0) {
              if (imports.length === 0) {
                actions.setSubmitting(false);
                actions.setErrors({ upload: 'No events to import.' });
              } else {
                const promises = imports.map(i => {
                  const eventData = Object.assign({}, i, {
                    body: JSON.stringify(i.body),
                    categories: [valuePerOptionFullName[i.categories]],
                  });
                  return window.api.postEvents(eventData);
                });
                try {
                  const newBackendEvents = await Promise.all(promises);
                  newBackendEvents.map(e => {
                    dispatch({
                      type: 'POST_EVENT',
                      payload: e.data,
                    });
                  });
                  const word =
                    newBackendEvents.length === 1 ? 'event' : 'events';
                  actions.setSubmitting(false);
                  alerts.success(
                    `Import successful: ${newBackendEvents.length} new ${word} created.`,
                  );
                  setVisible(false);
                } catch (apiError) {
                  actions.setSubmitting(false);
                  const transformedError = transformApiError(apiError);
                  actions.setErrors(transformedError);
                }
              }
            } else {
              actions.setSubmitting(false);
              actions.setErrors(validationErrors[0]);
            }
          });
        }
        reader.readAsText(file);
      }}
      validateOnBlur={false}
      validateOnChange={false}
      validationSchema={UploadSchema}
    >
      {({
        values,
        setFieldValue,
        errors,
        handleBlur,
        handleChange,
        handleSubmit,
        isSubmitting,
      }) => (
        <>
          <div>
            <Text strong>Notes on Importing Events</Text>
            <ul>
              <li>
                <Text>This form only accepts *.csv files.</Text>
              </li>
              <li>
                <Text>
                  There must be one full-name category per event, e.g.,{' '}
                </Text>
                <Text code>Model 3 &gt; Production Hell</Text>
                <Text>
                  . The full name includes the parent category and the
                  subcategories.
                </Text>
              </li>
              <li>
                <Text>
                  That full-name category must already exist. If you need to
                  create a new category, please use the category tool in the
                  navigation bar above.
                </Text>
              </li>
              <li>
                <Text>
                  Event times should contain a time zone. If they do not, they
                  will be assumed to use your time zone. The more detail here
                  the better. The best is ISO 8601 format:{' '}
                </Text>
                <Text code>2013-02-08T09:30:26+07</Text>
                <Text>.</Text>
              </li>
            </ul>
            <Text strong>Sample Data</Text>
            <Table
              dataSource={dataSource}
              columns={columns}
              size='small'
              pagination={false}
              bordered={true}
            />
          </div>
          <Form onSubmit={handleSubmit}>
            <Form.Item
              validateStatus={errors && errors.upload ? 'error' : ''}
              help={errors && errors.upload ? errors.upload : ''}
            >
              <Upload
                name='upload'
                accept='.csv,text/csv'
                onChange={handleChange}
                onBlur={handleBlur}
                fileList={values.upload}
                onRemove={_ => {
                  setFieldValue('upload', '');
                }}
                size='small'
                beforeUpload={(_, file) => {
                  setFieldValue('upload', file);
                  return false;
                }}
              >
                <Icon type='upload' /> Choose File
              </Upload>
            </Form.Item>
            <Form.Item>
              <Button size='small' type='primary' htmlType='submit'>
                Import
              </Button>
            </Form.Item>
            {isSubmitting && (
              <Form.Item>
                <Spin />
              </Form.Item>
            )}
          </Form>
        </>
      )}
    </Formik>
  );
};

export default ImportForm;
