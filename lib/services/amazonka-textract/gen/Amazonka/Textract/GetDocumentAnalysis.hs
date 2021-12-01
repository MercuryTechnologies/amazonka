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
-- Module      : Amazonka.Textract.GetDocumentAnalysis
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the results for an Amazon Textract asynchronous operation that
-- analyzes text in a document.
--
-- You start asynchronous text analysis by calling StartDocumentAnalysis,
-- which returns a job identifier (@JobId@). When the text analysis
-- operation finishes, Amazon Textract publishes a completion status to the
-- Amazon Simple Notification Service (Amazon SNS) topic that\'s registered
-- in the initial call to @StartDocumentAnalysis@. To get the results of
-- the text-detection operation, first check that the status value
-- published to the Amazon SNS topic is @SUCCEEDED@. If so, call
-- @GetDocumentAnalysis@, and pass the job identifier (@JobId@) from the
-- initial call to @StartDocumentAnalysis@.
--
-- @GetDocumentAnalysis@ returns an array of Block objects. The following
-- types of information are returned:
--
-- -   Form data (key-value pairs). The related information is returned in
--     two Block objects, each of type @KEY_VALUE_SET@: a KEY @Block@
--     object and a VALUE @Block@ object. For example, /Name: Ana Silva
--     Carolina/ contains a key and value. /Name:/ is the key. /Ana Silva
--     Carolina/ is the value.
--
-- -   Table and table cell data. A TABLE @Block@ object contains
--     information about a detected table. A CELL @Block@ object is
--     returned for each cell in a table.
--
-- -   Lines and words of text. A LINE @Block@ object contains one or more
--     WORD @Block@ objects. All lines and words that are detected in the
--     document are returned (including text that doesn\'t have a
--     relationship with the value of the @StartDocumentAnalysis@
--     @FeatureTypes@ input parameter).
--
-- Selection elements such as check boxes and option buttons (radio
-- buttons) can be detected in form data and in tables. A SELECTION_ELEMENT
-- @Block@ object contains information about a selection element, including
-- the selection status.
--
-- Use the @MaxResults@ parameter to limit the number of blocks that are
-- returned. If there are more results than specified in @MaxResults@, the
-- value of @NextToken@ in the operation response contains a pagination
-- token for getting the next set of results. To get the next page of
-- results, call @GetDocumentAnalysis@, and populate the @NextToken@
-- request parameter with the token value that\'s returned from the
-- previous call to @GetDocumentAnalysis@.
--
-- For more information, see
-- <https://docs.aws.amazon.com/textract/latest/dg/how-it-works-analyzing.html Document Text Analysis>.
module Amazonka.Textract.GetDocumentAnalysis
  ( -- * Creating a Request
    GetDocumentAnalysis (..),
    newGetDocumentAnalysis,

    -- * Request Lenses
    getDocumentAnalysis_nextToken,
    getDocumentAnalysis_maxResults,
    getDocumentAnalysis_jobId,

    -- * Destructuring the Response
    GetDocumentAnalysisResponse (..),
    newGetDocumentAnalysisResponse,

    -- * Response Lenses
    getDocumentAnalysisResponse_documentMetadata,
    getDocumentAnalysisResponse_blocks,
    getDocumentAnalysisResponse_analyzeDocumentModelVersion,
    getDocumentAnalysisResponse_warnings,
    getDocumentAnalysisResponse_nextToken,
    getDocumentAnalysisResponse_statusMessage,
    getDocumentAnalysisResponse_jobStatus,
    getDocumentAnalysisResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Textract.Types

-- | /See:/ 'newGetDocumentAnalysis' smart constructor.
data GetDocumentAnalysis = GetDocumentAnalysis'
  { -- | If the previous response was incomplete (because there are more blocks
    -- to retrieve), Amazon Textract returns a pagination token in the
    -- response. You can use this pagination token to retrieve the next set of
    -- blocks.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return per paginated call. The largest
    -- value that you can specify is 1,000. If you specify a value greater than
    -- 1,000, a maximum of 1,000 results is returned. The default value is
    -- 1,000.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A unique identifier for the text-detection job. The @JobId@ is returned
    -- from @StartDocumentAnalysis@. A @JobId@ value is only valid for 7 days.
    jobId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDocumentAnalysis' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getDocumentAnalysis_nextToken' - If the previous response was incomplete (because there are more blocks
-- to retrieve), Amazon Textract returns a pagination token in the
-- response. You can use this pagination token to retrieve the next set of
-- blocks.
--
-- 'maxResults', 'getDocumentAnalysis_maxResults' - The maximum number of results to return per paginated call. The largest
-- value that you can specify is 1,000. If you specify a value greater than
-- 1,000, a maximum of 1,000 results is returned. The default value is
-- 1,000.
--
-- 'jobId', 'getDocumentAnalysis_jobId' - A unique identifier for the text-detection job. The @JobId@ is returned
-- from @StartDocumentAnalysis@. A @JobId@ value is only valid for 7 days.
newGetDocumentAnalysis ::
  -- | 'jobId'
  Prelude.Text ->
  GetDocumentAnalysis
newGetDocumentAnalysis pJobId_ =
  GetDocumentAnalysis'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      jobId = pJobId_
    }

-- | If the previous response was incomplete (because there are more blocks
-- to retrieve), Amazon Textract returns a pagination token in the
-- response. You can use this pagination token to retrieve the next set of
-- blocks.
getDocumentAnalysis_nextToken :: Lens.Lens' GetDocumentAnalysis (Prelude.Maybe Prelude.Text)
getDocumentAnalysis_nextToken = Lens.lens (\GetDocumentAnalysis' {nextToken} -> nextToken) (\s@GetDocumentAnalysis' {} a -> s {nextToken = a} :: GetDocumentAnalysis)

-- | The maximum number of results to return per paginated call. The largest
-- value that you can specify is 1,000. If you specify a value greater than
-- 1,000, a maximum of 1,000 results is returned. The default value is
-- 1,000.
getDocumentAnalysis_maxResults :: Lens.Lens' GetDocumentAnalysis (Prelude.Maybe Prelude.Natural)
getDocumentAnalysis_maxResults = Lens.lens (\GetDocumentAnalysis' {maxResults} -> maxResults) (\s@GetDocumentAnalysis' {} a -> s {maxResults = a} :: GetDocumentAnalysis)

-- | A unique identifier for the text-detection job. The @JobId@ is returned
-- from @StartDocumentAnalysis@. A @JobId@ value is only valid for 7 days.
getDocumentAnalysis_jobId :: Lens.Lens' GetDocumentAnalysis Prelude.Text
getDocumentAnalysis_jobId = Lens.lens (\GetDocumentAnalysis' {jobId} -> jobId) (\s@GetDocumentAnalysis' {} a -> s {jobId = a} :: GetDocumentAnalysis)

instance Core.AWSRequest GetDocumentAnalysis where
  type
    AWSResponse GetDocumentAnalysis =
      GetDocumentAnalysisResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDocumentAnalysisResponse'
            Prelude.<$> (x Core..?> "DocumentMetadata")
            Prelude.<*> (x Core..?> "Blocks" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "AnalyzeDocumentModelVersion")
            Prelude.<*> (x Core..?> "Warnings" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "NextToken")
            Prelude.<*> (x Core..?> "StatusMessage")
            Prelude.<*> (x Core..?> "JobStatus")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetDocumentAnalysis where
  hashWithSalt salt' GetDocumentAnalysis' {..} =
    salt' `Prelude.hashWithSalt` jobId
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData GetDocumentAnalysis where
  rnf GetDocumentAnalysis' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf jobId
      `Prelude.seq` Prelude.rnf maxResults

instance Core.ToHeaders GetDocumentAnalysis where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Textract.GetDocumentAnalysis" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetDocumentAnalysis where
  toJSON GetDocumentAnalysis' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            Prelude.Just ("JobId" Core..= jobId)
          ]
      )

instance Core.ToPath GetDocumentAnalysis where
  toPath = Prelude.const "/"

instance Core.ToQuery GetDocumentAnalysis where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetDocumentAnalysisResponse' smart constructor.
data GetDocumentAnalysisResponse = GetDocumentAnalysisResponse'
  { -- | Information about a document that Amazon Textract processed.
    -- @DocumentMetadata@ is returned in every page of paginated responses from
    -- an Amazon Textract video operation.
    documentMetadata :: Prelude.Maybe DocumentMetadata,
    -- | The results of the text-analysis operation.
    blocks :: Prelude.Maybe [Block],
    analyzeDocumentModelVersion :: Prelude.Maybe Prelude.Text,
    -- | A list of warnings that occurred during the document-analysis operation.
    warnings :: Prelude.Maybe [Warning],
    -- | If the response is truncated, Amazon Textract returns this token. You
    -- can use this token in the subsequent request to retrieve the next set of
    -- text detection results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Returns if the detection job could not be completed. Contains
    -- explanation for what error occured.
    statusMessage :: Prelude.Maybe Prelude.Text,
    -- | The current status of the text detection job.
    jobStatus :: Prelude.Maybe JobStatus,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDocumentAnalysisResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'documentMetadata', 'getDocumentAnalysisResponse_documentMetadata' - Information about a document that Amazon Textract processed.
-- @DocumentMetadata@ is returned in every page of paginated responses from
-- an Amazon Textract video operation.
--
-- 'blocks', 'getDocumentAnalysisResponse_blocks' - The results of the text-analysis operation.
--
-- 'analyzeDocumentModelVersion', 'getDocumentAnalysisResponse_analyzeDocumentModelVersion' -
--
-- 'warnings', 'getDocumentAnalysisResponse_warnings' - A list of warnings that occurred during the document-analysis operation.
--
-- 'nextToken', 'getDocumentAnalysisResponse_nextToken' - If the response is truncated, Amazon Textract returns this token. You
-- can use this token in the subsequent request to retrieve the next set of
-- text detection results.
--
-- 'statusMessage', 'getDocumentAnalysisResponse_statusMessage' - Returns if the detection job could not be completed. Contains
-- explanation for what error occured.
--
-- 'jobStatus', 'getDocumentAnalysisResponse_jobStatus' - The current status of the text detection job.
--
-- 'httpStatus', 'getDocumentAnalysisResponse_httpStatus' - The response's http status code.
newGetDocumentAnalysisResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetDocumentAnalysisResponse
newGetDocumentAnalysisResponse pHttpStatus_ =
  GetDocumentAnalysisResponse'
    { documentMetadata =
        Prelude.Nothing,
      blocks = Prelude.Nothing,
      analyzeDocumentModelVersion = Prelude.Nothing,
      warnings = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      statusMessage = Prelude.Nothing,
      jobStatus = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about a document that Amazon Textract processed.
-- @DocumentMetadata@ is returned in every page of paginated responses from
-- an Amazon Textract video operation.
getDocumentAnalysisResponse_documentMetadata :: Lens.Lens' GetDocumentAnalysisResponse (Prelude.Maybe DocumentMetadata)
getDocumentAnalysisResponse_documentMetadata = Lens.lens (\GetDocumentAnalysisResponse' {documentMetadata} -> documentMetadata) (\s@GetDocumentAnalysisResponse' {} a -> s {documentMetadata = a} :: GetDocumentAnalysisResponse)

-- | The results of the text-analysis operation.
getDocumentAnalysisResponse_blocks :: Lens.Lens' GetDocumentAnalysisResponse (Prelude.Maybe [Block])
getDocumentAnalysisResponse_blocks = Lens.lens (\GetDocumentAnalysisResponse' {blocks} -> blocks) (\s@GetDocumentAnalysisResponse' {} a -> s {blocks = a} :: GetDocumentAnalysisResponse) Prelude.. Lens.mapping Lens.coerced

-- |
getDocumentAnalysisResponse_analyzeDocumentModelVersion :: Lens.Lens' GetDocumentAnalysisResponse (Prelude.Maybe Prelude.Text)
getDocumentAnalysisResponse_analyzeDocumentModelVersion = Lens.lens (\GetDocumentAnalysisResponse' {analyzeDocumentModelVersion} -> analyzeDocumentModelVersion) (\s@GetDocumentAnalysisResponse' {} a -> s {analyzeDocumentModelVersion = a} :: GetDocumentAnalysisResponse)

-- | A list of warnings that occurred during the document-analysis operation.
getDocumentAnalysisResponse_warnings :: Lens.Lens' GetDocumentAnalysisResponse (Prelude.Maybe [Warning])
getDocumentAnalysisResponse_warnings = Lens.lens (\GetDocumentAnalysisResponse' {warnings} -> warnings) (\s@GetDocumentAnalysisResponse' {} a -> s {warnings = a} :: GetDocumentAnalysisResponse) Prelude.. Lens.mapping Lens.coerced

-- | If the response is truncated, Amazon Textract returns this token. You
-- can use this token in the subsequent request to retrieve the next set of
-- text detection results.
getDocumentAnalysisResponse_nextToken :: Lens.Lens' GetDocumentAnalysisResponse (Prelude.Maybe Prelude.Text)
getDocumentAnalysisResponse_nextToken = Lens.lens (\GetDocumentAnalysisResponse' {nextToken} -> nextToken) (\s@GetDocumentAnalysisResponse' {} a -> s {nextToken = a} :: GetDocumentAnalysisResponse)

-- | Returns if the detection job could not be completed. Contains
-- explanation for what error occured.
getDocumentAnalysisResponse_statusMessage :: Lens.Lens' GetDocumentAnalysisResponse (Prelude.Maybe Prelude.Text)
getDocumentAnalysisResponse_statusMessage = Lens.lens (\GetDocumentAnalysisResponse' {statusMessage} -> statusMessage) (\s@GetDocumentAnalysisResponse' {} a -> s {statusMessage = a} :: GetDocumentAnalysisResponse)

-- | The current status of the text detection job.
getDocumentAnalysisResponse_jobStatus :: Lens.Lens' GetDocumentAnalysisResponse (Prelude.Maybe JobStatus)
getDocumentAnalysisResponse_jobStatus = Lens.lens (\GetDocumentAnalysisResponse' {jobStatus} -> jobStatus) (\s@GetDocumentAnalysisResponse' {} a -> s {jobStatus = a} :: GetDocumentAnalysisResponse)

-- | The response's http status code.
getDocumentAnalysisResponse_httpStatus :: Lens.Lens' GetDocumentAnalysisResponse Prelude.Int
getDocumentAnalysisResponse_httpStatus = Lens.lens (\GetDocumentAnalysisResponse' {httpStatus} -> httpStatus) (\s@GetDocumentAnalysisResponse' {} a -> s {httpStatus = a} :: GetDocumentAnalysisResponse)

instance Prelude.NFData GetDocumentAnalysisResponse where
  rnf GetDocumentAnalysisResponse' {..} =
    Prelude.rnf documentMetadata
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf jobStatus
      `Prelude.seq` Prelude.rnf statusMessage
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf warnings
      `Prelude.seq` Prelude.rnf analyzeDocumentModelVersion
      `Prelude.seq` Prelude.rnf blocks
