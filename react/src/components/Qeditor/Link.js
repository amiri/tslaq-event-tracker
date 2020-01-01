import React from 'react';
import { Editor, Range, Transforms } from 'slate';
import classnames from 'classnames';
import { Button } from 'antd';
import { isUrl } from './Utils';

const isLink = editor => {
  const [match] = Editor.nodes(editor, {
    match: n => n.type === 'link',
  });
  return !!match;
};

const insertLink = (editor, url) => {
  if (editor.selection) {
    wrapLink(editor, url);
  }
};

const unwrapLink = editor => {
  Transforms.unwrapNodes(editor, { match: n => n.type === 'link' });
};

const wrapLink = (editor, url) => {
  if (isLink(editor)) {
    unwrapLink(editor);
  }

  const { selection } = editor;
  const isCollapsed = selection && Range.isCollapsed(selection);
  const link = {
    type: 'link',
    url,
    children: isCollapsed ? [{ text: url }] : [],
  };

  if (isCollapsed) {
    Transforms.insertNodes(editor, link);
  } else {
    Transforms.wrapNodes(editor, link, { split: true });
    Transforms.collapse(editor, { edge: 'end' });
  }
};

export const withLinks = editor => {
  const { insertData, insertText, isInline } = editor;

  editor.isInline = element => {
    return element.type === 'link' ? true : isInline(element);
  };

  editor.insertText = text => {
    if (text && isUrl(text)) {
      wrapLink(editor, text);
    } else {
      insertText(text);
    }
  };

  editor.insertData = data => {
    const text = data.getData('text/plain');

    if (text && isUrl(text)) {
      wrapLink(editor, text);
    } else {
      insertData(data);
    }
  };

  return editor;
};

const prompt = window.prompt;

export const LinkButton = ({ editor }) => {
  return (
    <Button
      size='small'
      icon='link'
      onMouseDown={async e => {
        e.preventDefault();
        const isSet = isLink(editor);
        if (isSet) {
          unwrapLink(editor);
        } else {
          const url = prompt('Enter the URL');
          const isValid = isUrl(url);
          if (!url || !isValid) {
            return;
          }
          insertLink(editor, url);
        }
      }}
      className={classnames({
        active: isLink(editor),
      })}
    />
  );
};
