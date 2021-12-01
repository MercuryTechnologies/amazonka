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
-- Module      : Amazonka.Glue.ListBlueprints
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all the blueprint names in an account.
module Amazonka.Glue.ListBlueprints
  ( -- * Creating a Request
    ListBlueprints (..),
    newListBlueprints,

    -- * Request Lenses
    listBlueprints_nextToken,
    listBlueprints_maxResults,
    listBlueprints_tags,

    -- * Destructuring the Response
    ListBlueprintsResponse (..),
    newListBlueprintsResponse,

    -- * Response Lenses
    listBlueprintsResponse_blueprints,
    listBlueprintsResponse_nextToken,
    listBlueprintsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.Glue.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListBlueprints' smart constructor.
data ListBlueprints = ListBlueprints'
  { -- | A continuation token, if this is a continuation request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum size of a list to return.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Filters the list by an Amazon Web Services resource tag.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListBlueprints' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listBlueprints_nextToken' - A continuation token, if this is a continuation request.
--
-- 'maxResults', 'listBlueprints_maxResults' - The maximum size of a list to return.
--
-- 'tags', 'listBlueprints_tags' - Filters the list by an Amazon Web Services resource tag.
newListBlueprints ::
  ListBlueprints
newListBlueprints =
  ListBlueprints'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | A continuation token, if this is a continuation request.
listBlueprints_nextToken :: Lens.Lens' ListBlueprints (Prelude.Maybe Prelude.Text)
listBlueprints_nextToken = Lens.lens (\ListBlueprints' {nextToken} -> nextToken) (\s@ListBlueprints' {} a -> s {nextToken = a} :: ListBlueprints)

-- | The maximum size of a list to return.
listBlueprints_maxResults :: Lens.Lens' ListBlueprints (Prelude.Maybe Prelude.Natural)
listBlueprints_maxResults = Lens.lens (\ListBlueprints' {maxResults} -> maxResults) (\s@ListBlueprints' {} a -> s {maxResults = a} :: ListBlueprints)

-- | Filters the list by an Amazon Web Services resource tag.
listBlueprints_tags :: Lens.Lens' ListBlueprints (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
listBlueprints_tags = Lens.lens (\ListBlueprints' {tags} -> tags) (\s@ListBlueprints' {} a -> s {tags = a} :: ListBlueprints) Prelude.. Lens.mapping Lens.coerced

instance Core.AWSRequest ListBlueprints where
  type
    AWSResponse ListBlueprints =
      ListBlueprintsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListBlueprintsResponse'
            Prelude.<$> (x Core..?> "Blueprints" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListBlueprints where
  hashWithSalt salt' ListBlueprints' {..} =
    salt' `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListBlueprints where
  rnf ListBlueprints' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf maxResults

instance Core.ToHeaders ListBlueprints where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ("AWSGlue.ListBlueprints" :: Prelude.ByteString),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListBlueprints where
  toJSON ListBlueprints' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            ("Tags" Core..=) Prelude.<$> tags
          ]
      )

instance Core.ToPath ListBlueprints where
  toPath = Prelude.const "/"

instance Core.ToQuery ListBlueprints where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListBlueprintsResponse' smart constructor.
data ListBlueprintsResponse = ListBlueprintsResponse'
  { -- | List of names of blueprints in the account.
    blueprints :: Prelude.Maybe [Prelude.Text],
    -- | A continuation token, if not all blueprint names have been returned.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListBlueprintsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'blueprints', 'listBlueprintsResponse_blueprints' - List of names of blueprints in the account.
--
-- 'nextToken', 'listBlueprintsResponse_nextToken' - A continuation token, if not all blueprint names have been returned.
--
-- 'httpStatus', 'listBlueprintsResponse_httpStatus' - The response's http status code.
newListBlueprintsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListBlueprintsResponse
newListBlueprintsResponse pHttpStatus_ =
  ListBlueprintsResponse'
    { blueprints =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | List of names of blueprints in the account.
listBlueprintsResponse_blueprints :: Lens.Lens' ListBlueprintsResponse (Prelude.Maybe [Prelude.Text])
listBlueprintsResponse_blueprints = Lens.lens (\ListBlueprintsResponse' {blueprints} -> blueprints) (\s@ListBlueprintsResponse' {} a -> s {blueprints = a} :: ListBlueprintsResponse) Prelude.. Lens.mapping Lens.coerced

-- | A continuation token, if not all blueprint names have been returned.
listBlueprintsResponse_nextToken :: Lens.Lens' ListBlueprintsResponse (Prelude.Maybe Prelude.Text)
listBlueprintsResponse_nextToken = Lens.lens (\ListBlueprintsResponse' {nextToken} -> nextToken) (\s@ListBlueprintsResponse' {} a -> s {nextToken = a} :: ListBlueprintsResponse)

-- | The response's http status code.
listBlueprintsResponse_httpStatus :: Lens.Lens' ListBlueprintsResponse Prelude.Int
listBlueprintsResponse_httpStatus = Lens.lens (\ListBlueprintsResponse' {httpStatus} -> httpStatus) (\s@ListBlueprintsResponse' {} a -> s {httpStatus = a} :: ListBlueprintsResponse)

instance Prelude.NFData ListBlueprintsResponse where
  rnf ListBlueprintsResponse' {..} =
    Prelude.rnf blueprints
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf nextToken
