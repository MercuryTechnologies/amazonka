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
-- Module      : Amazonka.DataBrew.DescribeDataset
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the definition of a specific DataBrew dataset.
module Amazonka.DataBrew.DescribeDataset
  ( -- * Creating a Request
    DescribeDataset (..),
    newDescribeDataset,

    -- * Request Lenses
    describeDataset_name,

    -- * Destructuring the Response
    DescribeDatasetResponse (..),
    newDescribeDatasetResponse,

    -- * Response Lenses
    describeDatasetResponse_lastModifiedDate,
    describeDatasetResponse_pathOptions,
    describeDatasetResponse_createDate,
    describeDatasetResponse_formatOptions,
    describeDatasetResponse_format,
    describeDatasetResponse_createdBy,
    describeDatasetResponse_resourceArn,
    describeDatasetResponse_source,
    describeDatasetResponse_lastModifiedBy,
    describeDatasetResponse_tags,
    describeDatasetResponse_httpStatus,
    describeDatasetResponse_name,
    describeDatasetResponse_input,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.DataBrew.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeDataset' smart constructor.
data DescribeDataset = DescribeDataset'
  { -- | The name of the dataset to be described.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeDataset' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'describeDataset_name' - The name of the dataset to be described.
newDescribeDataset ::
  -- | 'name'
  Prelude.Text ->
  DescribeDataset
newDescribeDataset pName_ =
  DescribeDataset' {name = pName_}

-- | The name of the dataset to be described.
describeDataset_name :: Lens.Lens' DescribeDataset Prelude.Text
describeDataset_name = Lens.lens (\DescribeDataset' {name} -> name) (\s@DescribeDataset' {} a -> s {name = a} :: DescribeDataset)

instance Core.AWSRequest DescribeDataset where
  type
    AWSResponse DescribeDataset =
      DescribeDatasetResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeDatasetResponse'
            Prelude.<$> (x Core..?> "LastModifiedDate")
            Prelude.<*> (x Core..?> "PathOptions")
            Prelude.<*> (x Core..?> "CreateDate")
            Prelude.<*> (x Core..?> "FormatOptions")
            Prelude.<*> (x Core..?> "Format")
            Prelude.<*> (x Core..?> "CreatedBy")
            Prelude.<*> (x Core..?> "ResourceArn")
            Prelude.<*> (x Core..?> "Source")
            Prelude.<*> (x Core..?> "LastModifiedBy")
            Prelude.<*> (x Core..?> "Tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "Name")
            Prelude.<*> (x Core..:> "Input")
      )

instance Prelude.Hashable DescribeDataset where
  hashWithSalt salt' DescribeDataset' {..} =
    salt' `Prelude.hashWithSalt` name

instance Prelude.NFData DescribeDataset where
  rnf DescribeDataset' {..} = Prelude.rnf name

instance Core.ToHeaders DescribeDataset where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath DescribeDataset where
  toPath DescribeDataset' {..} =
    Prelude.mconcat ["/datasets/", Core.toBS name]

instance Core.ToQuery DescribeDataset where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeDatasetResponse' smart constructor.
data DescribeDatasetResponse = DescribeDatasetResponse'
  { -- | The date and time that the dataset was last modified.
    lastModifiedDate :: Prelude.Maybe Core.POSIX,
    -- | A set of options that defines how DataBrew interprets an Amazon S3 path
    -- of the dataset.
    pathOptions :: Prelude.Maybe PathOptions,
    -- | The date and time that the dataset was created.
    createDate :: Prelude.Maybe Core.POSIX,
    formatOptions :: Prelude.Maybe FormatOptions,
    -- | The file format of a dataset that is created from an Amazon S3 file or
    -- folder.
    format :: Prelude.Maybe InputFormat,
    -- | The identifier (user name) of the user who created the dataset.
    createdBy :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the dataset.
    resourceArn :: Prelude.Maybe Prelude.Text,
    -- | The location of the data for this dataset, Amazon S3 or the Glue Data
    -- Catalog.
    source :: Prelude.Maybe Source,
    -- | The identifier (user name) of the user who last modified the dataset.
    lastModifiedBy :: Prelude.Maybe Prelude.Text,
    -- | Metadata tags associated with this dataset.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The name of the dataset.
    name :: Prelude.Text,
    input :: Input
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeDatasetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastModifiedDate', 'describeDatasetResponse_lastModifiedDate' - The date and time that the dataset was last modified.
--
-- 'pathOptions', 'describeDatasetResponse_pathOptions' - A set of options that defines how DataBrew interprets an Amazon S3 path
-- of the dataset.
--
-- 'createDate', 'describeDatasetResponse_createDate' - The date and time that the dataset was created.
--
-- 'formatOptions', 'describeDatasetResponse_formatOptions' - Undocumented member.
--
-- 'format', 'describeDatasetResponse_format' - The file format of a dataset that is created from an Amazon S3 file or
-- folder.
--
-- 'createdBy', 'describeDatasetResponse_createdBy' - The identifier (user name) of the user who created the dataset.
--
-- 'resourceArn', 'describeDatasetResponse_resourceArn' - The Amazon Resource Name (ARN) of the dataset.
--
-- 'source', 'describeDatasetResponse_source' - The location of the data for this dataset, Amazon S3 or the Glue Data
-- Catalog.
--
-- 'lastModifiedBy', 'describeDatasetResponse_lastModifiedBy' - The identifier (user name) of the user who last modified the dataset.
--
-- 'tags', 'describeDatasetResponse_tags' - Metadata tags associated with this dataset.
--
-- 'httpStatus', 'describeDatasetResponse_httpStatus' - The response's http status code.
--
-- 'name', 'describeDatasetResponse_name' - The name of the dataset.
--
-- 'input', 'describeDatasetResponse_input' - Undocumented member.
newDescribeDatasetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'name'
  Prelude.Text ->
  -- | 'input'
  Input ->
  DescribeDatasetResponse
newDescribeDatasetResponse
  pHttpStatus_
  pName_
  pInput_ =
    DescribeDatasetResponse'
      { lastModifiedDate =
          Prelude.Nothing,
        pathOptions = Prelude.Nothing,
        createDate = Prelude.Nothing,
        formatOptions = Prelude.Nothing,
        format = Prelude.Nothing,
        createdBy = Prelude.Nothing,
        resourceArn = Prelude.Nothing,
        source = Prelude.Nothing,
        lastModifiedBy = Prelude.Nothing,
        tags = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        name = pName_,
        input = pInput_
      }

-- | The date and time that the dataset was last modified.
describeDatasetResponse_lastModifiedDate :: Lens.Lens' DescribeDatasetResponse (Prelude.Maybe Prelude.UTCTime)
describeDatasetResponse_lastModifiedDate = Lens.lens (\DescribeDatasetResponse' {lastModifiedDate} -> lastModifiedDate) (\s@DescribeDatasetResponse' {} a -> s {lastModifiedDate = a} :: DescribeDatasetResponse) Prelude.. Lens.mapping Core._Time

-- | A set of options that defines how DataBrew interprets an Amazon S3 path
-- of the dataset.
describeDatasetResponse_pathOptions :: Lens.Lens' DescribeDatasetResponse (Prelude.Maybe PathOptions)
describeDatasetResponse_pathOptions = Lens.lens (\DescribeDatasetResponse' {pathOptions} -> pathOptions) (\s@DescribeDatasetResponse' {} a -> s {pathOptions = a} :: DescribeDatasetResponse)

-- | The date and time that the dataset was created.
describeDatasetResponse_createDate :: Lens.Lens' DescribeDatasetResponse (Prelude.Maybe Prelude.UTCTime)
describeDatasetResponse_createDate = Lens.lens (\DescribeDatasetResponse' {createDate} -> createDate) (\s@DescribeDatasetResponse' {} a -> s {createDate = a} :: DescribeDatasetResponse) Prelude.. Lens.mapping Core._Time

-- | Undocumented member.
describeDatasetResponse_formatOptions :: Lens.Lens' DescribeDatasetResponse (Prelude.Maybe FormatOptions)
describeDatasetResponse_formatOptions = Lens.lens (\DescribeDatasetResponse' {formatOptions} -> formatOptions) (\s@DescribeDatasetResponse' {} a -> s {formatOptions = a} :: DescribeDatasetResponse)

-- | The file format of a dataset that is created from an Amazon S3 file or
-- folder.
describeDatasetResponse_format :: Lens.Lens' DescribeDatasetResponse (Prelude.Maybe InputFormat)
describeDatasetResponse_format = Lens.lens (\DescribeDatasetResponse' {format} -> format) (\s@DescribeDatasetResponse' {} a -> s {format = a} :: DescribeDatasetResponse)

-- | The identifier (user name) of the user who created the dataset.
describeDatasetResponse_createdBy :: Lens.Lens' DescribeDatasetResponse (Prelude.Maybe Prelude.Text)
describeDatasetResponse_createdBy = Lens.lens (\DescribeDatasetResponse' {createdBy} -> createdBy) (\s@DescribeDatasetResponse' {} a -> s {createdBy = a} :: DescribeDatasetResponse)

-- | The Amazon Resource Name (ARN) of the dataset.
describeDatasetResponse_resourceArn :: Lens.Lens' DescribeDatasetResponse (Prelude.Maybe Prelude.Text)
describeDatasetResponse_resourceArn = Lens.lens (\DescribeDatasetResponse' {resourceArn} -> resourceArn) (\s@DescribeDatasetResponse' {} a -> s {resourceArn = a} :: DescribeDatasetResponse)

-- | The location of the data for this dataset, Amazon S3 or the Glue Data
-- Catalog.
describeDatasetResponse_source :: Lens.Lens' DescribeDatasetResponse (Prelude.Maybe Source)
describeDatasetResponse_source = Lens.lens (\DescribeDatasetResponse' {source} -> source) (\s@DescribeDatasetResponse' {} a -> s {source = a} :: DescribeDatasetResponse)

-- | The identifier (user name) of the user who last modified the dataset.
describeDatasetResponse_lastModifiedBy :: Lens.Lens' DescribeDatasetResponse (Prelude.Maybe Prelude.Text)
describeDatasetResponse_lastModifiedBy = Lens.lens (\DescribeDatasetResponse' {lastModifiedBy} -> lastModifiedBy) (\s@DescribeDatasetResponse' {} a -> s {lastModifiedBy = a} :: DescribeDatasetResponse)

-- | Metadata tags associated with this dataset.
describeDatasetResponse_tags :: Lens.Lens' DescribeDatasetResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
describeDatasetResponse_tags = Lens.lens (\DescribeDatasetResponse' {tags} -> tags) (\s@DescribeDatasetResponse' {} a -> s {tags = a} :: DescribeDatasetResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeDatasetResponse_httpStatus :: Lens.Lens' DescribeDatasetResponse Prelude.Int
describeDatasetResponse_httpStatus = Lens.lens (\DescribeDatasetResponse' {httpStatus} -> httpStatus) (\s@DescribeDatasetResponse' {} a -> s {httpStatus = a} :: DescribeDatasetResponse)

-- | The name of the dataset.
describeDatasetResponse_name :: Lens.Lens' DescribeDatasetResponse Prelude.Text
describeDatasetResponse_name = Lens.lens (\DescribeDatasetResponse' {name} -> name) (\s@DescribeDatasetResponse' {} a -> s {name = a} :: DescribeDatasetResponse)

-- | Undocumented member.
describeDatasetResponse_input :: Lens.Lens' DescribeDatasetResponse Input
describeDatasetResponse_input = Lens.lens (\DescribeDatasetResponse' {input} -> input) (\s@DescribeDatasetResponse' {} a -> s {input = a} :: DescribeDatasetResponse)

instance Prelude.NFData DescribeDatasetResponse where
  rnf DescribeDatasetResponse' {..} =
    Prelude.rnf lastModifiedDate
      `Prelude.seq` Prelude.rnf input
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf lastModifiedBy
      `Prelude.seq` Prelude.rnf source
      `Prelude.seq` Prelude.rnf resourceArn
      `Prelude.seq` Prelude.rnf createdBy
      `Prelude.seq` Prelude.rnf format
      `Prelude.seq` Prelude.rnf formatOptions
      `Prelude.seq` Prelude.rnf createDate
      `Prelude.seq` Prelude.rnf pathOptions
