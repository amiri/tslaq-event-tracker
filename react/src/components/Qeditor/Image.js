import React from 'react';
import { Transforms } from 'slate';
import { isUrl } from './Utils';
import imageExtensions from 'image-extensions';
import { openImageUploadModal } from '../utils/Chart';
import { Button } from 'antd';
import { useHistory } from 'react-router';

export const ImageButton = ({ editor, eventId }) => {
  const history = useHistory();
  
  return (
    <Button
      size='small'
      icon='file-image'
      onMouseDown={e => {
        e.preventDefault();
        sessionStorage.setItem('imageInsertLocation', JSON.stringify(editor.selection));
        //console.log('setting imageInsertLocation: ', editor.selection);
        openImageUploadModal({ history, id: eventId });
      }}
    />
  );
};

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

export const insertImage = (editor, url, location) => {
  const text = { text: '' };
  const image = { type: 'image', url, children: [text] };
    //console.log('insertImage imageInsertLocation: ', location);
  try {
    Transforms.insertNodes(editor, image, (location && {at: location}));
    Transforms.insertNodes(editor, { type: 'paragraph', children: [text] });
    // console.log('insertImage successful');
  } catch (error) {
    console.error('insertImage: error: ', error);
  }
};

export const isImageUrl = url => {
  if (!url) return false;
  if (!isUrl(url)) return false;
  const ext = new URL(url).pathname.split('.').pop();
  return imageExtensions.includes(ext);
};
