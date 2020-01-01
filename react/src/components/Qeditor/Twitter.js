import React from 'react';
import { Transforms, Editor } from 'slate';
import { Button } from 'antd';
import { validTwitterUrl } from './Utils';
import classnames from 'classnames';

const isTwitter = editor => {
  const [match] = Editor.nodes(editor, {
    match: n => n.type === 'twitter',
  });
  return !!match;
};

const calculateTwitterDefs = url => {
  const captureRegex = RegExp(
    '^.+?(twitter.com)/(?<screenName>.+?)/(?<subtype>.+?)/(?<id>.+?)$',
  );
  return url.match(captureRegex).groups;
};

const insertTwitter = (editor, url) => {
  const twitterDefs = calculateTwitterDefs(url);
  const text = { text: '' };
  const twitter = {
    type: 'twitter',
    ...twitterDefs,
    url,
    children: [text],
  };
  Transforms.insertNodes(editor, twitter);
  Transforms.insertNodes(editor, { type: 'paragraph', children: [text] });
};

export const withTwitter = editor => {
  const { insertData, insertText } = editor;

  editor.insertText = text => {
    const valid = validTwitterUrl(text);
    if (text && valid) {
      insertTwitter(editor, text);
    } else {
      insertText(text);
    }
  };

  editor.insertData = data => {
    const text = data.getData('text/plain');
    const valid = validTwitterUrl(text);

    if (text && valid) {
      insertTwitter(editor, text);
    } else {
      insertData(data);
    }
  };

  return editor;
};

export const TwitterButton = ({ editor }) => {
  return (
    <Button
      size='small'
      icon='twitter'
      onMouseDown={e => {
        e.preventDefault();
        const url = prompt('Enter the tweet URL:');
        const isValid = validTwitterUrl(url);
        if (!url || !isValid) {
          return;
        }
        insertTwitter(editor, url);
      }}
      className={classnames({
        active: isTwitter(editor),
      })}
    />
  );
};
