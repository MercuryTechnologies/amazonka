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
-- Module      : Amazonka.Transcribe.ListVocabularies
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of vocabularies that match the specified criteria. If no
-- criteria are specified, returns the entire list of vocabularies.
module Amazonka.Transcribe.ListVocabularies
  ( -- * Creating a Request
    ListVocabularies (..),
    newListVocabularies,

    -- * Request Lenses
    listVocabularies_nameContains,
    listVocabularies_nextToken,
    listVocabularies_stateEquals,
    listVocabularies_maxResults,

    -- * Destructuring the Response
    ListVocabulariesResponse (..),
    newListVocabulariesResponse,

    -- * Response Lenses
    listVocabulariesResponse_vocabularies,
    listVocabulariesResponse_status,
    listVocabulariesResponse_nextToken,
    listVocabulariesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Transcribe.Types

-- | /See:/ 'newListVocabularies' smart constructor.
data ListVocabularies = ListVocabularies'
  { -- | When specified, the vocabularies returned in the list are limited to
    -- vocabularies whose name contains the specified string. The search is not
    -- case sensitive, @ListVocabularies@ returns both \"vocabularyname\" and
    -- \"VocabularyName\" in the response list.
    nameContains :: Prelude.Maybe Prelude.Text,
    -- | If the result of the previous request to @ListVocabularies@ was
    -- truncated, include the @NextToken@ to fetch the next set of jobs.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | When specified, only returns vocabularies with the @VocabularyState@
    -- field equal to the specified state.
    stateEquals :: Prelude.Maybe VocabularyState,
    -- | The maximum number of vocabularies to return in each page of results. If
    -- there are fewer results than the value you specify, only the actual
    -- results are returned. If you do not specify a value, the default of 5 is
    -- used.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListVocabularies' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nameContains', 'listVocabularies_nameContains' - When specified, the vocabularies returned in the list are limited to
-- vocabularies whose name contains the specified string. The search is not
-- case sensitive, @ListVocabularies@ returns both \"vocabularyname\" and
-- \"VocabularyName\" in the response list.
--
-- 'nextToken', 'listVocabularies_nextToken' - If the result of the previous request to @ListVocabularies@ was
-- truncated, include the @NextToken@ to fetch the next set of jobs.
--
-- 'stateEquals', 'listVocabularies_stateEquals' - When specified, only returns vocabularies with the @VocabularyState@
-- field equal to the specified state.
--
-- 'maxResults', 'listVocabularies_maxResults' - The maximum number of vocabularies to return in each page of results. If
-- there are fewer results than the value you specify, only the actual
-- results are returned. If you do not specify a value, the default of 5 is
-- used.
newListVocabularies ::
  ListVocabularies
newListVocabularies =
  ListVocabularies'
    { nameContains = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      stateEquals = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | When specified, the vocabularies returned in the list are limited to
-- vocabularies whose name contains the specified string. The search is not
-- case sensitive, @ListVocabularies@ returns both \"vocabularyname\" and
-- \"VocabularyName\" in the response list.
listVocabularies_nameContains :: Lens.Lens' ListVocabularies (Prelude.Maybe Prelude.Text)
listVocabularies_nameContains = Lens.lens (\ListVocabularies' {nameContains} -> nameContains) (\s@ListVocabularies' {} a -> s {nameContains = a} :: ListVocabularies)

-- | If the result of the previous request to @ListVocabularies@ was
-- truncated, include the @NextToken@ to fetch the next set of jobs.
listVocabularies_nextToken :: Lens.Lens' ListVocabularies (Prelude.Maybe Prelude.Text)
listVocabularies_nextToken = Lens.lens (\ListVocabularies' {nextToken} -> nextToken) (\s@ListVocabularies' {} a -> s {nextToken = a} :: ListVocabularies)

-- | When specified, only returns vocabularies with the @VocabularyState@
-- field equal to the specified state.
listVocabularies_stateEquals :: Lens.Lens' ListVocabularies (Prelude.Maybe VocabularyState)
listVocabularies_stateEquals = Lens.lens (\ListVocabularies' {stateEquals} -> stateEquals) (\s@ListVocabularies' {} a -> s {stateEquals = a} :: ListVocabularies)

-- | The maximum number of vocabularies to return in each page of results. If
-- there are fewer results than the value you specify, only the actual
-- results are returned. If you do not specify a value, the default of 5 is
-- used.
listVocabularies_maxResults :: Lens.Lens' ListVocabularies (Prelude.Maybe Prelude.Natural)
listVocabularies_maxResults = Lens.lens (\ListVocabularies' {maxResults} -> maxResults) (\s@ListVocabularies' {} a -> s {maxResults = a} :: ListVocabularies)

instance Core.AWSRequest ListVocabularies where
  type
    AWSResponse ListVocabularies =
      ListVocabulariesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListVocabulariesResponse'
            Prelude.<$> (x Core..?> "Vocabularies" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "Status")
            Prelude.<*> (x Core..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListVocabularies where
  hashWithSalt salt' ListVocabularies' {..} =
    salt' `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` stateEquals
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` nameContains

instance Prelude.NFData ListVocabularies where
  rnf ListVocabularies' {..} =
    Prelude.rnf nameContains
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf stateEquals
      `Prelude.seq` Prelude.rnf nextToken

instance Core.ToHeaders ListVocabularies where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Transcribe.ListVocabularies" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListVocabularies where
  toJSON ListVocabularies' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NameContains" Core..=) Prelude.<$> nameContains,
            ("NextToken" Core..=) Prelude.<$> nextToken,
            ("StateEquals" Core..=) Prelude.<$> stateEquals,
            ("MaxResults" Core..=) Prelude.<$> maxResults
          ]
      )

instance Core.ToPath ListVocabularies where
  toPath = Prelude.const "/"

instance Core.ToQuery ListVocabularies where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListVocabulariesResponse' smart constructor.
data ListVocabulariesResponse = ListVocabulariesResponse'
  { -- | A list of objects that describe the vocabularies that match the search
    -- criteria in the request.
    vocabularies :: Prelude.Maybe [VocabularyInfo],
    -- | The requested vocabulary state.
    status :: Prelude.Maybe VocabularyState,
    -- | The @ListVocabularies@ operation returns a page of vocabularies at a
    -- time. The maximum size of the page is set in the @MaxResults@ parameter.
    -- If there are more jobs in the list than will fit on the page, Amazon
    -- Transcribe returns the @NextPage@ token. To return in the next page of
    -- jobs, include the token in the next request to the @ListVocabularies@
    -- operation.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListVocabulariesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vocabularies', 'listVocabulariesResponse_vocabularies' - A list of objects that describe the vocabularies that match the search
-- criteria in the request.
--
-- 'status', 'listVocabulariesResponse_status' - The requested vocabulary state.
--
-- 'nextToken', 'listVocabulariesResponse_nextToken' - The @ListVocabularies@ operation returns a page of vocabularies at a
-- time. The maximum size of the page is set in the @MaxResults@ parameter.
-- If there are more jobs in the list than will fit on the page, Amazon
-- Transcribe returns the @NextPage@ token. To return in the next page of
-- jobs, include the token in the next request to the @ListVocabularies@
-- operation.
--
-- 'httpStatus', 'listVocabulariesResponse_httpStatus' - The response's http status code.
newListVocabulariesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListVocabulariesResponse
newListVocabulariesResponse pHttpStatus_ =
  ListVocabulariesResponse'
    { vocabularies =
        Prelude.Nothing,
      status = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of objects that describe the vocabularies that match the search
-- criteria in the request.
listVocabulariesResponse_vocabularies :: Lens.Lens' ListVocabulariesResponse (Prelude.Maybe [VocabularyInfo])
listVocabulariesResponse_vocabularies = Lens.lens (\ListVocabulariesResponse' {vocabularies} -> vocabularies) (\s@ListVocabulariesResponse' {} a -> s {vocabularies = a} :: ListVocabulariesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The requested vocabulary state.
listVocabulariesResponse_status :: Lens.Lens' ListVocabulariesResponse (Prelude.Maybe VocabularyState)
listVocabulariesResponse_status = Lens.lens (\ListVocabulariesResponse' {status} -> status) (\s@ListVocabulariesResponse' {} a -> s {status = a} :: ListVocabulariesResponse)

-- | The @ListVocabularies@ operation returns a page of vocabularies at a
-- time. The maximum size of the page is set in the @MaxResults@ parameter.
-- If there are more jobs in the list than will fit on the page, Amazon
-- Transcribe returns the @NextPage@ token. To return in the next page of
-- jobs, include the token in the next request to the @ListVocabularies@
-- operation.
listVocabulariesResponse_nextToken :: Lens.Lens' ListVocabulariesResponse (Prelude.Maybe Prelude.Text)
listVocabulariesResponse_nextToken = Lens.lens (\ListVocabulariesResponse' {nextToken} -> nextToken) (\s@ListVocabulariesResponse' {} a -> s {nextToken = a} :: ListVocabulariesResponse)

-- | The response's http status code.
listVocabulariesResponse_httpStatus :: Lens.Lens' ListVocabulariesResponse Prelude.Int
listVocabulariesResponse_httpStatus = Lens.lens (\ListVocabulariesResponse' {httpStatus} -> httpStatus) (\s@ListVocabulariesResponse' {} a -> s {httpStatus = a} :: ListVocabulariesResponse)

instance Prelude.NFData ListVocabulariesResponse where
  rnf ListVocabulariesResponse' {..} =
    Prelude.rnf vocabularies
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf status
