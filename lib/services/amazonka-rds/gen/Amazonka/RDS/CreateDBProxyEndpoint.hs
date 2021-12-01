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
-- Module      : Amazonka.RDS.CreateDBProxyEndpoint
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a @DBProxyEndpoint@. Only applies to proxies that are associated
-- with Aurora DB clusters. You can use DB proxy endpoints to specify
-- read\/write or read-only access to the DB cluster. You can also use DB
-- proxy endpoints to access a DB proxy through a different VPC than the
-- proxy\'s default VPC.
module Amazonka.RDS.CreateDBProxyEndpoint
  ( -- * Creating a Request
    CreateDBProxyEndpoint (..),
    newCreateDBProxyEndpoint,

    -- * Request Lenses
    createDBProxyEndpoint_targetRole,
    createDBProxyEndpoint_vpcSecurityGroupIds,
    createDBProxyEndpoint_tags,
    createDBProxyEndpoint_dbProxyName,
    createDBProxyEndpoint_dbProxyEndpointName,
    createDBProxyEndpoint_vpcSubnetIds,

    -- * Destructuring the Response
    CreateDBProxyEndpointResponse (..),
    newCreateDBProxyEndpointResponse,

    -- * Response Lenses
    createDBProxyEndpointResponse_dbProxyEndpoint,
    createDBProxyEndpointResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.RDS.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateDBProxyEndpoint' smart constructor.
data CreateDBProxyEndpoint = CreateDBProxyEndpoint'
  { -- | A value that indicates whether the DB proxy endpoint can be used for
    -- read\/write or read-only operations. The default is @READ_WRITE@.
    targetRole :: Prelude.Maybe DBProxyEndpointTargetRole,
    -- | The VPC security group IDs for the DB proxy endpoint that you create.
    -- You can specify a different set of security group IDs than for the
    -- original DB proxy. The default is the default security group for the
    -- VPC.
    vpcSecurityGroupIds :: Prelude.Maybe [Prelude.Text],
    tags :: Prelude.Maybe [Tag],
    -- | The name of the DB proxy associated with the DB proxy endpoint that you
    -- create.
    dbProxyName :: Prelude.Text,
    -- | The name of the DB proxy endpoint to create.
    dbProxyEndpointName :: Prelude.Text,
    -- | The VPC subnet IDs for the DB proxy endpoint that you create. You can
    -- specify a different set of subnet IDs than for the original DB proxy.
    vpcSubnetIds :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDBProxyEndpoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'targetRole', 'createDBProxyEndpoint_targetRole' - A value that indicates whether the DB proxy endpoint can be used for
-- read\/write or read-only operations. The default is @READ_WRITE@.
--
-- 'vpcSecurityGroupIds', 'createDBProxyEndpoint_vpcSecurityGroupIds' - The VPC security group IDs for the DB proxy endpoint that you create.
-- You can specify a different set of security group IDs than for the
-- original DB proxy. The default is the default security group for the
-- VPC.
--
-- 'tags', 'createDBProxyEndpoint_tags' - Undocumented member.
--
-- 'dbProxyName', 'createDBProxyEndpoint_dbProxyName' - The name of the DB proxy associated with the DB proxy endpoint that you
-- create.
--
-- 'dbProxyEndpointName', 'createDBProxyEndpoint_dbProxyEndpointName' - The name of the DB proxy endpoint to create.
--
-- 'vpcSubnetIds', 'createDBProxyEndpoint_vpcSubnetIds' - The VPC subnet IDs for the DB proxy endpoint that you create. You can
-- specify a different set of subnet IDs than for the original DB proxy.
newCreateDBProxyEndpoint ::
  -- | 'dbProxyName'
  Prelude.Text ->
  -- | 'dbProxyEndpointName'
  Prelude.Text ->
  CreateDBProxyEndpoint
newCreateDBProxyEndpoint
  pDBProxyName_
  pDBProxyEndpointName_ =
    CreateDBProxyEndpoint'
      { targetRole =
          Prelude.Nothing,
        vpcSecurityGroupIds = Prelude.Nothing,
        tags = Prelude.Nothing,
        dbProxyName = pDBProxyName_,
        dbProxyEndpointName = pDBProxyEndpointName_,
        vpcSubnetIds = Prelude.mempty
      }

-- | A value that indicates whether the DB proxy endpoint can be used for
-- read\/write or read-only operations. The default is @READ_WRITE@.
createDBProxyEndpoint_targetRole :: Lens.Lens' CreateDBProxyEndpoint (Prelude.Maybe DBProxyEndpointTargetRole)
createDBProxyEndpoint_targetRole = Lens.lens (\CreateDBProxyEndpoint' {targetRole} -> targetRole) (\s@CreateDBProxyEndpoint' {} a -> s {targetRole = a} :: CreateDBProxyEndpoint)

-- | The VPC security group IDs for the DB proxy endpoint that you create.
-- You can specify a different set of security group IDs than for the
-- original DB proxy. The default is the default security group for the
-- VPC.
createDBProxyEndpoint_vpcSecurityGroupIds :: Lens.Lens' CreateDBProxyEndpoint (Prelude.Maybe [Prelude.Text])
createDBProxyEndpoint_vpcSecurityGroupIds = Lens.lens (\CreateDBProxyEndpoint' {vpcSecurityGroupIds} -> vpcSecurityGroupIds) (\s@CreateDBProxyEndpoint' {} a -> s {vpcSecurityGroupIds = a} :: CreateDBProxyEndpoint) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
createDBProxyEndpoint_tags :: Lens.Lens' CreateDBProxyEndpoint (Prelude.Maybe [Tag])
createDBProxyEndpoint_tags = Lens.lens (\CreateDBProxyEndpoint' {tags} -> tags) (\s@CreateDBProxyEndpoint' {} a -> s {tags = a} :: CreateDBProxyEndpoint) Prelude.. Lens.mapping Lens.coerced

-- | The name of the DB proxy associated with the DB proxy endpoint that you
-- create.
createDBProxyEndpoint_dbProxyName :: Lens.Lens' CreateDBProxyEndpoint Prelude.Text
createDBProxyEndpoint_dbProxyName = Lens.lens (\CreateDBProxyEndpoint' {dbProxyName} -> dbProxyName) (\s@CreateDBProxyEndpoint' {} a -> s {dbProxyName = a} :: CreateDBProxyEndpoint)

-- | The name of the DB proxy endpoint to create.
createDBProxyEndpoint_dbProxyEndpointName :: Lens.Lens' CreateDBProxyEndpoint Prelude.Text
createDBProxyEndpoint_dbProxyEndpointName = Lens.lens (\CreateDBProxyEndpoint' {dbProxyEndpointName} -> dbProxyEndpointName) (\s@CreateDBProxyEndpoint' {} a -> s {dbProxyEndpointName = a} :: CreateDBProxyEndpoint)

-- | The VPC subnet IDs for the DB proxy endpoint that you create. You can
-- specify a different set of subnet IDs than for the original DB proxy.
createDBProxyEndpoint_vpcSubnetIds :: Lens.Lens' CreateDBProxyEndpoint [Prelude.Text]
createDBProxyEndpoint_vpcSubnetIds = Lens.lens (\CreateDBProxyEndpoint' {vpcSubnetIds} -> vpcSubnetIds) (\s@CreateDBProxyEndpoint' {} a -> s {vpcSubnetIds = a} :: CreateDBProxyEndpoint) Prelude.. Lens.coerced

instance Core.AWSRequest CreateDBProxyEndpoint where
  type
    AWSResponse CreateDBProxyEndpoint =
      CreateDBProxyEndpointResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "CreateDBProxyEndpointResult"
      ( \s h x ->
          CreateDBProxyEndpointResponse'
            Prelude.<$> (x Core..@? "DBProxyEndpoint")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateDBProxyEndpoint where
  hashWithSalt salt' CreateDBProxyEndpoint' {..} =
    salt' `Prelude.hashWithSalt` vpcSubnetIds
      `Prelude.hashWithSalt` dbProxyEndpointName
      `Prelude.hashWithSalt` dbProxyName
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` vpcSecurityGroupIds
      `Prelude.hashWithSalt` targetRole

instance Prelude.NFData CreateDBProxyEndpoint where
  rnf CreateDBProxyEndpoint' {..} =
    Prelude.rnf targetRole
      `Prelude.seq` Prelude.rnf vpcSubnetIds
      `Prelude.seq` Prelude.rnf dbProxyEndpointName
      `Prelude.seq` Prelude.rnf dbProxyName
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf vpcSecurityGroupIds

instance Core.ToHeaders CreateDBProxyEndpoint where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath CreateDBProxyEndpoint where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateDBProxyEndpoint where
  toQuery CreateDBProxyEndpoint' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("CreateDBProxyEndpoint" :: Prelude.ByteString),
        "Version"
          Core.=: ("2014-10-31" :: Prelude.ByteString),
        "TargetRole" Core.=: targetRole,
        "VpcSecurityGroupIds"
          Core.=: Core.toQuery
            ( Core.toQueryList "member"
                Prelude.<$> vpcSecurityGroupIds
            ),
        "Tags"
          Core.=: Core.toQuery
            (Core.toQueryList "Tag" Prelude.<$> tags),
        "DBProxyName" Core.=: dbProxyName,
        "DBProxyEndpointName" Core.=: dbProxyEndpointName,
        "VpcSubnetIds"
          Core.=: Core.toQueryList "member" vpcSubnetIds
      ]

-- | /See:/ 'newCreateDBProxyEndpointResponse' smart constructor.
data CreateDBProxyEndpointResponse = CreateDBProxyEndpointResponse'
  { -- | The @DBProxyEndpoint@ object that is created by the API operation. The
    -- DB proxy endpoint that you create might provide capabilities such as
    -- read\/write or read-only operations, or using a different VPC than the
    -- proxy\'s default VPC.
    dbProxyEndpoint :: Prelude.Maybe DBProxyEndpoint,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDBProxyEndpointResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dbProxyEndpoint', 'createDBProxyEndpointResponse_dbProxyEndpoint' - The @DBProxyEndpoint@ object that is created by the API operation. The
-- DB proxy endpoint that you create might provide capabilities such as
-- read\/write or read-only operations, or using a different VPC than the
-- proxy\'s default VPC.
--
-- 'httpStatus', 'createDBProxyEndpointResponse_httpStatus' - The response's http status code.
newCreateDBProxyEndpointResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateDBProxyEndpointResponse
newCreateDBProxyEndpointResponse pHttpStatus_ =
  CreateDBProxyEndpointResponse'
    { dbProxyEndpoint =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The @DBProxyEndpoint@ object that is created by the API operation. The
-- DB proxy endpoint that you create might provide capabilities such as
-- read\/write or read-only operations, or using a different VPC than the
-- proxy\'s default VPC.
createDBProxyEndpointResponse_dbProxyEndpoint :: Lens.Lens' CreateDBProxyEndpointResponse (Prelude.Maybe DBProxyEndpoint)
createDBProxyEndpointResponse_dbProxyEndpoint = Lens.lens (\CreateDBProxyEndpointResponse' {dbProxyEndpoint} -> dbProxyEndpoint) (\s@CreateDBProxyEndpointResponse' {} a -> s {dbProxyEndpoint = a} :: CreateDBProxyEndpointResponse)

-- | The response's http status code.
createDBProxyEndpointResponse_httpStatus :: Lens.Lens' CreateDBProxyEndpointResponse Prelude.Int
createDBProxyEndpointResponse_httpStatus = Lens.lens (\CreateDBProxyEndpointResponse' {httpStatus} -> httpStatus) (\s@CreateDBProxyEndpointResponse' {} a -> s {httpStatus = a} :: CreateDBProxyEndpointResponse)

instance Prelude.NFData CreateDBProxyEndpointResponse where
  rnf CreateDBProxyEndpointResponse' {..} =
    Prelude.rnf dbProxyEndpoint
      `Prelude.seq` Prelude.rnf httpStatus
