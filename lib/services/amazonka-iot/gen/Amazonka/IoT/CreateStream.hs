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
-- Module      : Amazonka.IoT.CreateStream
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a stream for delivering one or more large files in chunks over
-- MQTT. A stream transports data bytes in chunks or blocks packaged as
-- MQTT messages from a source like S3. You can have one or more files
-- associated with a stream.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions CreateStream>
-- action.
module Amazonka.IoT.CreateStream
  ( -- * Creating a Request
    CreateStream (..),
    newCreateStream,

    -- * Request Lenses
    createStream_description,
    createStream_tags,
    createStream_streamId,
    createStream_files,
    createStream_roleArn,

    -- * Destructuring the Response
    CreateStreamResponse (..),
    newCreateStreamResponse,

    -- * Response Lenses
    createStreamResponse_streamVersion,
    createStreamResponse_streamArn,
    createStreamResponse_description,
    createStreamResponse_streamId,
    createStreamResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.IoT.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateStream' smart constructor.
data CreateStream = CreateStream'
  { -- | A description of the stream.
    description :: Prelude.Maybe Prelude.Text,
    -- | Metadata which can be used to manage streams.
    tags :: Prelude.Maybe [Tag],
    -- | The stream ID.
    streamId :: Prelude.Text,
    -- | The files to stream.
    files :: Prelude.NonEmpty StreamFile,
    -- | An IAM role that allows the IoT service principal assumes to access your
    -- S3 files.
    roleArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateStream' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'createStream_description' - A description of the stream.
--
-- 'tags', 'createStream_tags' - Metadata which can be used to manage streams.
--
-- 'streamId', 'createStream_streamId' - The stream ID.
--
-- 'files', 'createStream_files' - The files to stream.
--
-- 'roleArn', 'createStream_roleArn' - An IAM role that allows the IoT service principal assumes to access your
-- S3 files.
newCreateStream ::
  -- | 'streamId'
  Prelude.Text ->
  -- | 'files'
  Prelude.NonEmpty StreamFile ->
  -- | 'roleArn'
  Prelude.Text ->
  CreateStream
newCreateStream pStreamId_ pFiles_ pRoleArn_ =
  CreateStream'
    { description = Prelude.Nothing,
      tags = Prelude.Nothing,
      streamId = pStreamId_,
      files = Lens.coerced Lens.# pFiles_,
      roleArn = pRoleArn_
    }

-- | A description of the stream.
createStream_description :: Lens.Lens' CreateStream (Prelude.Maybe Prelude.Text)
createStream_description = Lens.lens (\CreateStream' {description} -> description) (\s@CreateStream' {} a -> s {description = a} :: CreateStream)

-- | Metadata which can be used to manage streams.
createStream_tags :: Lens.Lens' CreateStream (Prelude.Maybe [Tag])
createStream_tags = Lens.lens (\CreateStream' {tags} -> tags) (\s@CreateStream' {} a -> s {tags = a} :: CreateStream) Prelude.. Lens.mapping Lens.coerced

-- | The stream ID.
createStream_streamId :: Lens.Lens' CreateStream Prelude.Text
createStream_streamId = Lens.lens (\CreateStream' {streamId} -> streamId) (\s@CreateStream' {} a -> s {streamId = a} :: CreateStream)

-- | The files to stream.
createStream_files :: Lens.Lens' CreateStream (Prelude.NonEmpty StreamFile)
createStream_files = Lens.lens (\CreateStream' {files} -> files) (\s@CreateStream' {} a -> s {files = a} :: CreateStream) Prelude.. Lens.coerced

-- | An IAM role that allows the IoT service principal assumes to access your
-- S3 files.
createStream_roleArn :: Lens.Lens' CreateStream Prelude.Text
createStream_roleArn = Lens.lens (\CreateStream' {roleArn} -> roleArn) (\s@CreateStream' {} a -> s {roleArn = a} :: CreateStream)

instance Core.AWSRequest CreateStream where
  type AWSResponse CreateStream = CreateStreamResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateStreamResponse'
            Prelude.<$> (x Core..?> "streamVersion")
            Prelude.<*> (x Core..?> "streamArn")
            Prelude.<*> (x Core..?> "description")
            Prelude.<*> (x Core..?> "streamId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateStream where
  hashWithSalt salt' CreateStream' {..} =
    salt' `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` files
      `Prelude.hashWithSalt` streamId
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` description

instance Prelude.NFData CreateStream where
  rnf CreateStream' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf files
      `Prelude.seq` Prelude.rnf streamId
      `Prelude.seq` Prelude.rnf tags

instance Core.ToHeaders CreateStream where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToJSON CreateStream where
  toJSON CreateStream' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("description" Core..=) Prelude.<$> description,
            ("tags" Core..=) Prelude.<$> tags,
            Prelude.Just ("files" Core..= files),
            Prelude.Just ("roleArn" Core..= roleArn)
          ]
      )

instance Core.ToPath CreateStream where
  toPath CreateStream' {..} =
    Prelude.mconcat ["/streams/", Core.toBS streamId]

instance Core.ToQuery CreateStream where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateStreamResponse' smart constructor.
data CreateStreamResponse = CreateStreamResponse'
  { -- | The version of the stream.
    streamVersion :: Prelude.Maybe Prelude.Natural,
    -- | The stream ARN.
    streamArn :: Prelude.Maybe Prelude.Text,
    -- | A description of the stream.
    description :: Prelude.Maybe Prelude.Text,
    -- | The stream ID.
    streamId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateStreamResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'streamVersion', 'createStreamResponse_streamVersion' - The version of the stream.
--
-- 'streamArn', 'createStreamResponse_streamArn' - The stream ARN.
--
-- 'description', 'createStreamResponse_description' - A description of the stream.
--
-- 'streamId', 'createStreamResponse_streamId' - The stream ID.
--
-- 'httpStatus', 'createStreamResponse_httpStatus' - The response's http status code.
newCreateStreamResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateStreamResponse
newCreateStreamResponse pHttpStatus_ =
  CreateStreamResponse'
    { streamVersion =
        Prelude.Nothing,
      streamArn = Prelude.Nothing,
      description = Prelude.Nothing,
      streamId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The version of the stream.
createStreamResponse_streamVersion :: Lens.Lens' CreateStreamResponse (Prelude.Maybe Prelude.Natural)
createStreamResponse_streamVersion = Lens.lens (\CreateStreamResponse' {streamVersion} -> streamVersion) (\s@CreateStreamResponse' {} a -> s {streamVersion = a} :: CreateStreamResponse)

-- | The stream ARN.
createStreamResponse_streamArn :: Lens.Lens' CreateStreamResponse (Prelude.Maybe Prelude.Text)
createStreamResponse_streamArn = Lens.lens (\CreateStreamResponse' {streamArn} -> streamArn) (\s@CreateStreamResponse' {} a -> s {streamArn = a} :: CreateStreamResponse)

-- | A description of the stream.
createStreamResponse_description :: Lens.Lens' CreateStreamResponse (Prelude.Maybe Prelude.Text)
createStreamResponse_description = Lens.lens (\CreateStreamResponse' {description} -> description) (\s@CreateStreamResponse' {} a -> s {description = a} :: CreateStreamResponse)

-- | The stream ID.
createStreamResponse_streamId :: Lens.Lens' CreateStreamResponse (Prelude.Maybe Prelude.Text)
createStreamResponse_streamId = Lens.lens (\CreateStreamResponse' {streamId} -> streamId) (\s@CreateStreamResponse' {} a -> s {streamId = a} :: CreateStreamResponse)

-- | The response's http status code.
createStreamResponse_httpStatus :: Lens.Lens' CreateStreamResponse Prelude.Int
createStreamResponse_httpStatus = Lens.lens (\CreateStreamResponse' {httpStatus} -> httpStatus) (\s@CreateStreamResponse' {} a -> s {httpStatus = a} :: CreateStreamResponse)

instance Prelude.NFData CreateStreamResponse where
  rnf CreateStreamResponse' {..} =
    Prelude.rnf streamVersion
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf streamId
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf streamArn
