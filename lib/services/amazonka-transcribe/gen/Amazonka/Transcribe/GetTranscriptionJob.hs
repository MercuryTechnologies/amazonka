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
-- Module      : Amazonka.Transcribe.GetTranscriptionJob
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a transcription job. To see the status of the
-- job, check the @TranscriptionJobStatus@ field. If the status is
-- @COMPLETED@, the job is finished and you can find the results at the
-- location specified in the @TranscriptFileUri@ field. If you enable
-- content redaction, the redacted transcript appears in
-- @RedactedTranscriptFileUri@.
module Amazonka.Transcribe.GetTranscriptionJob
  ( -- * Creating a Request
    GetTranscriptionJob (..),
    newGetTranscriptionJob,

    -- * Request Lenses
    getTranscriptionJob_transcriptionJobName,

    -- * Destructuring the Response
    GetTranscriptionJobResponse (..),
    newGetTranscriptionJobResponse,

    -- * Response Lenses
    getTranscriptionJobResponse_transcriptionJob,
    getTranscriptionJobResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Transcribe.Types

-- | /See:/ 'newGetTranscriptionJob' smart constructor.
data GetTranscriptionJob = GetTranscriptionJob'
  { -- | The name of the job.
    transcriptionJobName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetTranscriptionJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'transcriptionJobName', 'getTranscriptionJob_transcriptionJobName' - The name of the job.
newGetTranscriptionJob ::
  -- | 'transcriptionJobName'
  Prelude.Text ->
  GetTranscriptionJob
newGetTranscriptionJob pTranscriptionJobName_ =
  GetTranscriptionJob'
    { transcriptionJobName =
        pTranscriptionJobName_
    }

-- | The name of the job.
getTranscriptionJob_transcriptionJobName :: Lens.Lens' GetTranscriptionJob Prelude.Text
getTranscriptionJob_transcriptionJobName = Lens.lens (\GetTranscriptionJob' {transcriptionJobName} -> transcriptionJobName) (\s@GetTranscriptionJob' {} a -> s {transcriptionJobName = a} :: GetTranscriptionJob)

instance Core.AWSRequest GetTranscriptionJob where
  type
    AWSResponse GetTranscriptionJob =
      GetTranscriptionJobResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetTranscriptionJobResponse'
            Prelude.<$> (x Core..?> "TranscriptionJob")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetTranscriptionJob where
  hashWithSalt salt' GetTranscriptionJob' {..} =
    salt' `Prelude.hashWithSalt` transcriptionJobName

instance Prelude.NFData GetTranscriptionJob where
  rnf GetTranscriptionJob' {..} =
    Prelude.rnf transcriptionJobName

instance Core.ToHeaders GetTranscriptionJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Transcribe.GetTranscriptionJob" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetTranscriptionJob where
  toJSON GetTranscriptionJob' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "TranscriptionJobName"
                  Core..= transcriptionJobName
              )
          ]
      )

instance Core.ToPath GetTranscriptionJob where
  toPath = Prelude.const "/"

instance Core.ToQuery GetTranscriptionJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetTranscriptionJobResponse' smart constructor.
data GetTranscriptionJobResponse = GetTranscriptionJobResponse'
  { -- | An object that contains the results of the transcription job.
    transcriptionJob :: Prelude.Maybe TranscriptionJob,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetTranscriptionJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'transcriptionJob', 'getTranscriptionJobResponse_transcriptionJob' - An object that contains the results of the transcription job.
--
-- 'httpStatus', 'getTranscriptionJobResponse_httpStatus' - The response's http status code.
newGetTranscriptionJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetTranscriptionJobResponse
newGetTranscriptionJobResponse pHttpStatus_ =
  GetTranscriptionJobResponse'
    { transcriptionJob =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An object that contains the results of the transcription job.
getTranscriptionJobResponse_transcriptionJob :: Lens.Lens' GetTranscriptionJobResponse (Prelude.Maybe TranscriptionJob)
getTranscriptionJobResponse_transcriptionJob = Lens.lens (\GetTranscriptionJobResponse' {transcriptionJob} -> transcriptionJob) (\s@GetTranscriptionJobResponse' {} a -> s {transcriptionJob = a} :: GetTranscriptionJobResponse)

-- | The response's http status code.
getTranscriptionJobResponse_httpStatus :: Lens.Lens' GetTranscriptionJobResponse Prelude.Int
getTranscriptionJobResponse_httpStatus = Lens.lens (\GetTranscriptionJobResponse' {httpStatus} -> httpStatus) (\s@GetTranscriptionJobResponse' {} a -> s {httpStatus = a} :: GetTranscriptionJobResponse)

instance Prelude.NFData GetTranscriptionJobResponse where
  rnf GetTranscriptionJobResponse' {..} =
    Prelude.rnf transcriptionJob
      `Prelude.seq` Prelude.rnf httpStatus
