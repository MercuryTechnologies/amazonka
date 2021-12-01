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
-- Module      : Amazonka.IoTAnalytics.StartPipelineReprocessing
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts the reprocessing of raw message data through the pipeline.
module Amazonka.IoTAnalytics.StartPipelineReprocessing
  ( -- * Creating a Request
    StartPipelineReprocessing (..),
    newStartPipelineReprocessing,

    -- * Request Lenses
    startPipelineReprocessing_startTime,
    startPipelineReprocessing_endTime,
    startPipelineReprocessing_channelMessages,
    startPipelineReprocessing_pipelineName,

    -- * Destructuring the Response
    StartPipelineReprocessingResponse (..),
    newStartPipelineReprocessingResponse,

    -- * Response Lenses
    startPipelineReprocessingResponse_reprocessingId,
    startPipelineReprocessingResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.IoTAnalytics.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartPipelineReprocessing' smart constructor.
data StartPipelineReprocessing = StartPipelineReprocessing'
  { -- | The start time (inclusive) of raw message data that is reprocessed.
    --
    -- If you specify a value for the @startTime@ parameter, you must not use
    -- the @channelMessages@ object.
    startTime :: Prelude.Maybe Core.POSIX,
    -- | The end time (exclusive) of raw message data that is reprocessed.
    --
    -- If you specify a value for the @endTime@ parameter, you must not use the
    -- @channelMessages@ object.
    endTime :: Prelude.Maybe Core.POSIX,
    -- | Specifies one or more sets of channel messages that you want to
    -- reprocess.
    --
    -- If you use the @channelMessages@ object, you must not specify a value
    -- for @startTime@ and @endTime@.
    channelMessages :: Prelude.Maybe ChannelMessages,
    -- | The name of the pipeline on which to start reprocessing.
    pipelineName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartPipelineReprocessing' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'startTime', 'startPipelineReprocessing_startTime' - The start time (inclusive) of raw message data that is reprocessed.
--
-- If you specify a value for the @startTime@ parameter, you must not use
-- the @channelMessages@ object.
--
-- 'endTime', 'startPipelineReprocessing_endTime' - The end time (exclusive) of raw message data that is reprocessed.
--
-- If you specify a value for the @endTime@ parameter, you must not use the
-- @channelMessages@ object.
--
-- 'channelMessages', 'startPipelineReprocessing_channelMessages' - Specifies one or more sets of channel messages that you want to
-- reprocess.
--
-- If you use the @channelMessages@ object, you must not specify a value
-- for @startTime@ and @endTime@.
--
-- 'pipelineName', 'startPipelineReprocessing_pipelineName' - The name of the pipeline on which to start reprocessing.
newStartPipelineReprocessing ::
  -- | 'pipelineName'
  Prelude.Text ->
  StartPipelineReprocessing
newStartPipelineReprocessing pPipelineName_ =
  StartPipelineReprocessing'
    { startTime =
        Prelude.Nothing,
      endTime = Prelude.Nothing,
      channelMessages = Prelude.Nothing,
      pipelineName = pPipelineName_
    }

-- | The start time (inclusive) of raw message data that is reprocessed.
--
-- If you specify a value for the @startTime@ parameter, you must not use
-- the @channelMessages@ object.
startPipelineReprocessing_startTime :: Lens.Lens' StartPipelineReprocessing (Prelude.Maybe Prelude.UTCTime)
startPipelineReprocessing_startTime = Lens.lens (\StartPipelineReprocessing' {startTime} -> startTime) (\s@StartPipelineReprocessing' {} a -> s {startTime = a} :: StartPipelineReprocessing) Prelude.. Lens.mapping Core._Time

-- | The end time (exclusive) of raw message data that is reprocessed.
--
-- If you specify a value for the @endTime@ parameter, you must not use the
-- @channelMessages@ object.
startPipelineReprocessing_endTime :: Lens.Lens' StartPipelineReprocessing (Prelude.Maybe Prelude.UTCTime)
startPipelineReprocessing_endTime = Lens.lens (\StartPipelineReprocessing' {endTime} -> endTime) (\s@StartPipelineReprocessing' {} a -> s {endTime = a} :: StartPipelineReprocessing) Prelude.. Lens.mapping Core._Time

-- | Specifies one or more sets of channel messages that you want to
-- reprocess.
--
-- If you use the @channelMessages@ object, you must not specify a value
-- for @startTime@ and @endTime@.
startPipelineReprocessing_channelMessages :: Lens.Lens' StartPipelineReprocessing (Prelude.Maybe ChannelMessages)
startPipelineReprocessing_channelMessages = Lens.lens (\StartPipelineReprocessing' {channelMessages} -> channelMessages) (\s@StartPipelineReprocessing' {} a -> s {channelMessages = a} :: StartPipelineReprocessing)

-- | The name of the pipeline on which to start reprocessing.
startPipelineReprocessing_pipelineName :: Lens.Lens' StartPipelineReprocessing Prelude.Text
startPipelineReprocessing_pipelineName = Lens.lens (\StartPipelineReprocessing' {pipelineName} -> pipelineName) (\s@StartPipelineReprocessing' {} a -> s {pipelineName = a} :: StartPipelineReprocessing)

instance Core.AWSRequest StartPipelineReprocessing where
  type
    AWSResponse StartPipelineReprocessing =
      StartPipelineReprocessingResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          StartPipelineReprocessingResponse'
            Prelude.<$> (x Core..?> "reprocessingId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartPipelineReprocessing where
  hashWithSalt salt' StartPipelineReprocessing' {..} =
    salt' `Prelude.hashWithSalt` pipelineName
      `Prelude.hashWithSalt` channelMessages
      `Prelude.hashWithSalt` endTime
      `Prelude.hashWithSalt` startTime

instance Prelude.NFData StartPipelineReprocessing where
  rnf StartPipelineReprocessing' {..} =
    Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf pipelineName
      `Prelude.seq` Prelude.rnf channelMessages
      `Prelude.seq` Prelude.rnf endTime

instance Core.ToHeaders StartPipelineReprocessing where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToJSON StartPipelineReprocessing where
  toJSON StartPipelineReprocessing' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("startTime" Core..=) Prelude.<$> startTime,
            ("endTime" Core..=) Prelude.<$> endTime,
            ("channelMessages" Core..=)
              Prelude.<$> channelMessages
          ]
      )

instance Core.ToPath StartPipelineReprocessing where
  toPath StartPipelineReprocessing' {..} =
    Prelude.mconcat
      [ "/pipelines/",
        Core.toBS pipelineName,
        "/reprocessing"
      ]

instance Core.ToQuery StartPipelineReprocessing where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartPipelineReprocessingResponse' smart constructor.
data StartPipelineReprocessingResponse = StartPipelineReprocessingResponse'
  { -- | The ID of the pipeline reprocessing activity that was started.
    reprocessingId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartPipelineReprocessingResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'reprocessingId', 'startPipelineReprocessingResponse_reprocessingId' - The ID of the pipeline reprocessing activity that was started.
--
-- 'httpStatus', 'startPipelineReprocessingResponse_httpStatus' - The response's http status code.
newStartPipelineReprocessingResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartPipelineReprocessingResponse
newStartPipelineReprocessingResponse pHttpStatus_ =
  StartPipelineReprocessingResponse'
    { reprocessingId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID of the pipeline reprocessing activity that was started.
startPipelineReprocessingResponse_reprocessingId :: Lens.Lens' StartPipelineReprocessingResponse (Prelude.Maybe Prelude.Text)
startPipelineReprocessingResponse_reprocessingId = Lens.lens (\StartPipelineReprocessingResponse' {reprocessingId} -> reprocessingId) (\s@StartPipelineReprocessingResponse' {} a -> s {reprocessingId = a} :: StartPipelineReprocessingResponse)

-- | The response's http status code.
startPipelineReprocessingResponse_httpStatus :: Lens.Lens' StartPipelineReprocessingResponse Prelude.Int
startPipelineReprocessingResponse_httpStatus = Lens.lens (\StartPipelineReprocessingResponse' {httpStatus} -> httpStatus) (\s@StartPipelineReprocessingResponse' {} a -> s {httpStatus = a} :: StartPipelineReprocessingResponse)

instance
  Prelude.NFData
    StartPipelineReprocessingResponse
  where
  rnf StartPipelineReprocessingResponse' {..} =
    Prelude.rnf reprocessingId
      `Prelude.seq` Prelude.rnf httpStatus
