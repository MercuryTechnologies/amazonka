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
-- Module      : Amazonka.MediaPackageVOD.DescribePackagingConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a description of a MediaPackage VOD PackagingConfiguration
-- resource.
module Amazonka.MediaPackageVOD.DescribePackagingConfiguration
  ( -- * Creating a Request
    DescribePackagingConfiguration (..),
    newDescribePackagingConfiguration,

    -- * Request Lenses
    describePackagingConfiguration_id,

    -- * Destructuring the Response
    DescribePackagingConfigurationResponse (..),
    newDescribePackagingConfigurationResponse,

    -- * Response Lenses
    describePackagingConfigurationResponse_hlsPackage,
    describePackagingConfigurationResponse_arn,
    describePackagingConfigurationResponse_packagingGroupId,
    describePackagingConfigurationResponse_dashPackage,
    describePackagingConfigurationResponse_mssPackage,
    describePackagingConfigurationResponse_id,
    describePackagingConfigurationResponse_cmafPackage,
    describePackagingConfigurationResponse_tags,
    describePackagingConfigurationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.MediaPackageVOD.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribePackagingConfiguration' smart constructor.
data DescribePackagingConfiguration = DescribePackagingConfiguration'
  { -- | The ID of a MediaPackage VOD PackagingConfiguration resource.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribePackagingConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'describePackagingConfiguration_id' - The ID of a MediaPackage VOD PackagingConfiguration resource.
newDescribePackagingConfiguration ::
  -- | 'id'
  Prelude.Text ->
  DescribePackagingConfiguration
newDescribePackagingConfiguration pId_ =
  DescribePackagingConfiguration' {id = pId_}

-- | The ID of a MediaPackage VOD PackagingConfiguration resource.
describePackagingConfiguration_id :: Lens.Lens' DescribePackagingConfiguration Prelude.Text
describePackagingConfiguration_id = Lens.lens (\DescribePackagingConfiguration' {id} -> id) (\s@DescribePackagingConfiguration' {} a -> s {id = a} :: DescribePackagingConfiguration)

instance
  Core.AWSRequest
    DescribePackagingConfiguration
  where
  type
    AWSResponse DescribePackagingConfiguration =
      DescribePackagingConfigurationResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribePackagingConfigurationResponse'
            Prelude.<$> (x Core..?> "hlsPackage")
            Prelude.<*> (x Core..?> "arn")
            Prelude.<*> (x Core..?> "packagingGroupId")
            Prelude.<*> (x Core..?> "dashPackage")
            Prelude.<*> (x Core..?> "mssPackage")
            Prelude.<*> (x Core..?> "id")
            Prelude.<*> (x Core..?> "cmafPackage")
            Prelude.<*> (x Core..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribePackagingConfiguration
  where
  hashWithSalt
    salt'
    DescribePackagingConfiguration' {..} =
      salt' `Prelude.hashWithSalt` id

instance
  Prelude.NFData
    DescribePackagingConfiguration
  where
  rnf DescribePackagingConfiguration' {..} =
    Prelude.rnf id

instance
  Core.ToHeaders
    DescribePackagingConfiguration
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath DescribePackagingConfiguration where
  toPath DescribePackagingConfiguration' {..} =
    Prelude.mconcat
      ["/packaging_configurations/", Core.toBS id]

instance Core.ToQuery DescribePackagingConfiguration where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribePackagingConfigurationResponse' smart constructor.
data DescribePackagingConfigurationResponse = DescribePackagingConfigurationResponse'
  { hlsPackage :: Prelude.Maybe HlsPackage,
    -- | The ARN of the PackagingConfiguration.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The ID of a PackagingGroup.
    packagingGroupId :: Prelude.Maybe Prelude.Text,
    dashPackage :: Prelude.Maybe DashPackage,
    mssPackage :: Prelude.Maybe MssPackage,
    -- | The ID of the PackagingConfiguration.
    id :: Prelude.Maybe Prelude.Text,
    cmafPackage :: Prelude.Maybe CmafPackage,
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribePackagingConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hlsPackage', 'describePackagingConfigurationResponse_hlsPackage' - Undocumented member.
--
-- 'arn', 'describePackagingConfigurationResponse_arn' - The ARN of the PackagingConfiguration.
--
-- 'packagingGroupId', 'describePackagingConfigurationResponse_packagingGroupId' - The ID of a PackagingGroup.
--
-- 'dashPackage', 'describePackagingConfigurationResponse_dashPackage' - Undocumented member.
--
-- 'mssPackage', 'describePackagingConfigurationResponse_mssPackage' - Undocumented member.
--
-- 'id', 'describePackagingConfigurationResponse_id' - The ID of the PackagingConfiguration.
--
-- 'cmafPackage', 'describePackagingConfigurationResponse_cmafPackage' - Undocumented member.
--
-- 'tags', 'describePackagingConfigurationResponse_tags' - Undocumented member.
--
-- 'httpStatus', 'describePackagingConfigurationResponse_httpStatus' - The response's http status code.
newDescribePackagingConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribePackagingConfigurationResponse
newDescribePackagingConfigurationResponse
  pHttpStatus_ =
    DescribePackagingConfigurationResponse'
      { hlsPackage =
          Prelude.Nothing,
        arn = Prelude.Nothing,
        packagingGroupId = Prelude.Nothing,
        dashPackage = Prelude.Nothing,
        mssPackage = Prelude.Nothing,
        id = Prelude.Nothing,
        cmafPackage = Prelude.Nothing,
        tags = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Undocumented member.
describePackagingConfigurationResponse_hlsPackage :: Lens.Lens' DescribePackagingConfigurationResponse (Prelude.Maybe HlsPackage)
describePackagingConfigurationResponse_hlsPackage = Lens.lens (\DescribePackagingConfigurationResponse' {hlsPackage} -> hlsPackage) (\s@DescribePackagingConfigurationResponse' {} a -> s {hlsPackage = a} :: DescribePackagingConfigurationResponse)

-- | The ARN of the PackagingConfiguration.
describePackagingConfigurationResponse_arn :: Lens.Lens' DescribePackagingConfigurationResponse (Prelude.Maybe Prelude.Text)
describePackagingConfigurationResponse_arn = Lens.lens (\DescribePackagingConfigurationResponse' {arn} -> arn) (\s@DescribePackagingConfigurationResponse' {} a -> s {arn = a} :: DescribePackagingConfigurationResponse)

-- | The ID of a PackagingGroup.
describePackagingConfigurationResponse_packagingGroupId :: Lens.Lens' DescribePackagingConfigurationResponse (Prelude.Maybe Prelude.Text)
describePackagingConfigurationResponse_packagingGroupId = Lens.lens (\DescribePackagingConfigurationResponse' {packagingGroupId} -> packagingGroupId) (\s@DescribePackagingConfigurationResponse' {} a -> s {packagingGroupId = a} :: DescribePackagingConfigurationResponse)

-- | Undocumented member.
describePackagingConfigurationResponse_dashPackage :: Lens.Lens' DescribePackagingConfigurationResponse (Prelude.Maybe DashPackage)
describePackagingConfigurationResponse_dashPackage = Lens.lens (\DescribePackagingConfigurationResponse' {dashPackage} -> dashPackage) (\s@DescribePackagingConfigurationResponse' {} a -> s {dashPackage = a} :: DescribePackagingConfigurationResponse)

-- | Undocumented member.
describePackagingConfigurationResponse_mssPackage :: Lens.Lens' DescribePackagingConfigurationResponse (Prelude.Maybe MssPackage)
describePackagingConfigurationResponse_mssPackage = Lens.lens (\DescribePackagingConfigurationResponse' {mssPackage} -> mssPackage) (\s@DescribePackagingConfigurationResponse' {} a -> s {mssPackage = a} :: DescribePackagingConfigurationResponse)

-- | The ID of the PackagingConfiguration.
describePackagingConfigurationResponse_id :: Lens.Lens' DescribePackagingConfigurationResponse (Prelude.Maybe Prelude.Text)
describePackagingConfigurationResponse_id = Lens.lens (\DescribePackagingConfigurationResponse' {id} -> id) (\s@DescribePackagingConfigurationResponse' {} a -> s {id = a} :: DescribePackagingConfigurationResponse)

-- | Undocumented member.
describePackagingConfigurationResponse_cmafPackage :: Lens.Lens' DescribePackagingConfigurationResponse (Prelude.Maybe CmafPackage)
describePackagingConfigurationResponse_cmafPackage = Lens.lens (\DescribePackagingConfigurationResponse' {cmafPackage} -> cmafPackage) (\s@DescribePackagingConfigurationResponse' {} a -> s {cmafPackage = a} :: DescribePackagingConfigurationResponse)

-- | Undocumented member.
describePackagingConfigurationResponse_tags :: Lens.Lens' DescribePackagingConfigurationResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
describePackagingConfigurationResponse_tags = Lens.lens (\DescribePackagingConfigurationResponse' {tags} -> tags) (\s@DescribePackagingConfigurationResponse' {} a -> s {tags = a} :: DescribePackagingConfigurationResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describePackagingConfigurationResponse_httpStatus :: Lens.Lens' DescribePackagingConfigurationResponse Prelude.Int
describePackagingConfigurationResponse_httpStatus = Lens.lens (\DescribePackagingConfigurationResponse' {httpStatus} -> httpStatus) (\s@DescribePackagingConfigurationResponse' {} a -> s {httpStatus = a} :: DescribePackagingConfigurationResponse)

instance
  Prelude.NFData
    DescribePackagingConfigurationResponse
  where
  rnf DescribePackagingConfigurationResponse' {..} =
    Prelude.rnf hlsPackage
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf cmafPackage
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf mssPackage
      `Prelude.seq` Prelude.rnf dashPackage
      `Prelude.seq` Prelude.rnf packagingGroupId
      `Prelude.seq` Prelude.rnf arn
