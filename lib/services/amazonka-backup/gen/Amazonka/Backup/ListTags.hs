{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Backup.ListTags
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of key-value pairs assigned to a target recovery point,
-- backup plan, or backup vault.
--
-- @ListTags@ are currently only supported with Amazon EFS backups.
module Amazonka.Backup.ListTags
  ( -- * Creating a Request
    ListTags (..),
    newListTags,

    -- * Request Lenses
    listTags_nextToken,
    listTags_maxResults,
    listTags_resourceArn,

    -- * Destructuring the Response
    ListTagsResponse (..),
    newListTagsResponse,

    -- * Response Lenses
    listTagsResponse_nextToken,
    listTagsResponse_tags,
    listTagsResponse_httpStatus,
  )
where

import Amazonka.Backup.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListTags' smart constructor.
data ListTags = ListTags'
  { -- | The next item following a partial list of returned items. For example,
    -- if a request is made to return @maxResults@ number of items, @NextToken@
    -- allows you to return more items in your list starting at the location
    -- pointed to by the next token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of items to be returned.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | An Amazon Resource Name (ARN) that uniquely identifies a resource. The
    -- format of the ARN depends on the type of resource. Valid targets for
    -- @ListTags@ are recovery points, backup plans, and backup vaults.
    resourceArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTags' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listTags_nextToken' - The next item following a partial list of returned items. For example,
-- if a request is made to return @maxResults@ number of items, @NextToken@
-- allows you to return more items in your list starting at the location
-- pointed to by the next token.
--
-- 'maxResults', 'listTags_maxResults' - The maximum number of items to be returned.
--
-- 'resourceArn', 'listTags_resourceArn' - An Amazon Resource Name (ARN) that uniquely identifies a resource. The
-- format of the ARN depends on the type of resource. Valid targets for
-- @ListTags@ are recovery points, backup plans, and backup vaults.
newListTags ::
  -- | 'resourceArn'
  Prelude.Text ->
  ListTags
newListTags pResourceArn_ =
  ListTags'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      resourceArn = pResourceArn_
    }

-- | The next item following a partial list of returned items. For example,
-- if a request is made to return @maxResults@ number of items, @NextToken@
-- allows you to return more items in your list starting at the location
-- pointed to by the next token.
listTags_nextToken :: Lens.Lens' ListTags (Prelude.Maybe Prelude.Text)
listTags_nextToken = Lens.lens (\ListTags' {nextToken} -> nextToken) (\s@ListTags' {} a -> s {nextToken = a} :: ListTags)

-- | The maximum number of items to be returned.
listTags_maxResults :: Lens.Lens' ListTags (Prelude.Maybe Prelude.Natural)
listTags_maxResults = Lens.lens (\ListTags' {maxResults} -> maxResults) (\s@ListTags' {} a -> s {maxResults = a} :: ListTags)

-- | An Amazon Resource Name (ARN) that uniquely identifies a resource. The
-- format of the ARN depends on the type of resource. Valid targets for
-- @ListTags@ are recovery points, backup plans, and backup vaults.
listTags_resourceArn :: Lens.Lens' ListTags Prelude.Text
listTags_resourceArn = Lens.lens (\ListTags' {resourceArn} -> resourceArn) (\s@ListTags' {} a -> s {resourceArn = a} :: ListTags)

instance Core.AWSRequest ListTags where
  type AWSResponse ListTags = ListTagsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTagsResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (x Core..?> "Tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListTags where
  hashWithSalt salt' ListTags' {..} =
    salt' `Prelude.hashWithSalt` resourceArn
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListTags where
  rnf ListTags' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf resourceArn
      `Prelude.seq` Prelude.rnf maxResults

instance Core.ToHeaders ListTags where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath ListTags where
  toPath ListTags' {..} =
    Prelude.mconcat
      ["/tags/", Core.toBS resourceArn, "/"]

instance Core.ToQuery ListTags where
  toQuery ListTags' {..} =
    Prelude.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults
      ]

-- | /See:/ 'newListTagsResponse' smart constructor.
data ListTagsResponse = ListTagsResponse'
  { -- | The next item following a partial list of returned items. For example,
    -- if a request is made to return @maxResults@ number of items, @NextToken@
    -- allows you to return more items in your list starting at the location
    -- pointed to by the next token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | To help organize your resources, you can assign your own metadata to the
    -- resources you create. Each tag is a key-value pair.
    tags :: Prelude.Maybe (Core.Sensitive (Prelude.HashMap Prelude.Text Prelude.Text)),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTagsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listTagsResponse_nextToken' - The next item following a partial list of returned items. For example,
-- if a request is made to return @maxResults@ number of items, @NextToken@
-- allows you to return more items in your list starting at the location
-- pointed to by the next token.
--
-- 'tags', 'listTagsResponse_tags' - To help organize your resources, you can assign your own metadata to the
-- resources you create. Each tag is a key-value pair.
--
-- 'httpStatus', 'listTagsResponse_httpStatus' - The response's http status code.
newListTagsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListTagsResponse
newListTagsResponse pHttpStatus_ =
  ListTagsResponse'
    { nextToken = Prelude.Nothing,
      tags = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The next item following a partial list of returned items. For example,
-- if a request is made to return @maxResults@ number of items, @NextToken@
-- allows you to return more items in your list starting at the location
-- pointed to by the next token.
listTagsResponse_nextToken :: Lens.Lens' ListTagsResponse (Prelude.Maybe Prelude.Text)
listTagsResponse_nextToken = Lens.lens (\ListTagsResponse' {nextToken} -> nextToken) (\s@ListTagsResponse' {} a -> s {nextToken = a} :: ListTagsResponse)

-- | To help organize your resources, you can assign your own metadata to the
-- resources you create. Each tag is a key-value pair.
listTagsResponse_tags :: Lens.Lens' ListTagsResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
listTagsResponse_tags = Lens.lens (\ListTagsResponse' {tags} -> tags) (\s@ListTagsResponse' {} a -> s {tags = a} :: ListTagsResponse) Prelude.. Lens.mapping (Core._Sensitive Prelude.. Lens.coerced)

-- | The response's http status code.
listTagsResponse_httpStatus :: Lens.Lens' ListTagsResponse Prelude.Int
listTagsResponse_httpStatus = Lens.lens (\ListTagsResponse' {httpStatus} -> httpStatus) (\s@ListTagsResponse' {} a -> s {httpStatus = a} :: ListTagsResponse)

instance Prelude.NFData ListTagsResponse where
  rnf ListTagsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf tags
