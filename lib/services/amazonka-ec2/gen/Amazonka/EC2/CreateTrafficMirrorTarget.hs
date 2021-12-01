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
-- Module      : Amazonka.EC2.CreateTrafficMirrorTarget
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a target for your Traffic Mirror session.
--
-- A Traffic Mirror target is the destination for mirrored traffic. The
-- Traffic Mirror source and the Traffic Mirror target (monitoring
-- appliances) can be in the same VPC, or in different VPCs connected via
-- VPC peering or a transit gateway.
--
-- A Traffic Mirror target can be a network interface, or a Network Load
-- Balancer.
--
-- To use the target in a Traffic Mirror session, use
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateTrafficMirrorSession.htm CreateTrafficMirrorSession>.
module Amazonka.EC2.CreateTrafficMirrorTarget
  ( -- * Creating a Request
    CreateTrafficMirrorTarget (..),
    newCreateTrafficMirrorTarget,

    -- * Request Lenses
    createTrafficMirrorTarget_clientToken,
    createTrafficMirrorTarget_networkInterfaceId,
    createTrafficMirrorTarget_networkLoadBalancerArn,
    createTrafficMirrorTarget_tagSpecifications,
    createTrafficMirrorTarget_description,
    createTrafficMirrorTarget_dryRun,

    -- * Destructuring the Response
    CreateTrafficMirrorTargetResponse (..),
    newCreateTrafficMirrorTargetResponse,

    -- * Response Lenses
    createTrafficMirrorTargetResponse_clientToken,
    createTrafficMirrorTargetResponse_trafficMirrorTarget,
    createTrafficMirrorTargetResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.EC2.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateTrafficMirrorTarget' smart constructor.
data CreateTrafficMirrorTarget = CreateTrafficMirrorTarget'
  { -- | Unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to ensure idempotency>.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The network interface ID that is associated with the target.
    networkInterfaceId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the Network Load Balancer that is
    -- associated with the target.
    networkLoadBalancerArn :: Prelude.Maybe Prelude.Text,
    -- | The tags to assign to the Traffic Mirror target.
    tagSpecifications :: Prelude.Maybe [TagSpecification],
    -- | The description of the Traffic Mirror target.
    description :: Prelude.Maybe Prelude.Text,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateTrafficMirrorTarget' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'createTrafficMirrorTarget_clientToken' - Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to ensure idempotency>.
--
-- 'networkInterfaceId', 'createTrafficMirrorTarget_networkInterfaceId' - The network interface ID that is associated with the target.
--
-- 'networkLoadBalancerArn', 'createTrafficMirrorTarget_networkLoadBalancerArn' - The Amazon Resource Name (ARN) of the Network Load Balancer that is
-- associated with the target.
--
-- 'tagSpecifications', 'createTrafficMirrorTarget_tagSpecifications' - The tags to assign to the Traffic Mirror target.
--
-- 'description', 'createTrafficMirrorTarget_description' - The description of the Traffic Mirror target.
--
-- 'dryRun', 'createTrafficMirrorTarget_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
newCreateTrafficMirrorTarget ::
  CreateTrafficMirrorTarget
newCreateTrafficMirrorTarget =
  CreateTrafficMirrorTarget'
    { clientToken =
        Prelude.Nothing,
      networkInterfaceId = Prelude.Nothing,
      networkLoadBalancerArn = Prelude.Nothing,
      tagSpecifications = Prelude.Nothing,
      description = Prelude.Nothing,
      dryRun = Prelude.Nothing
    }

-- | Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to ensure idempotency>.
createTrafficMirrorTarget_clientToken :: Lens.Lens' CreateTrafficMirrorTarget (Prelude.Maybe Prelude.Text)
createTrafficMirrorTarget_clientToken = Lens.lens (\CreateTrafficMirrorTarget' {clientToken} -> clientToken) (\s@CreateTrafficMirrorTarget' {} a -> s {clientToken = a} :: CreateTrafficMirrorTarget)

-- | The network interface ID that is associated with the target.
createTrafficMirrorTarget_networkInterfaceId :: Lens.Lens' CreateTrafficMirrorTarget (Prelude.Maybe Prelude.Text)
createTrafficMirrorTarget_networkInterfaceId = Lens.lens (\CreateTrafficMirrorTarget' {networkInterfaceId} -> networkInterfaceId) (\s@CreateTrafficMirrorTarget' {} a -> s {networkInterfaceId = a} :: CreateTrafficMirrorTarget)

-- | The Amazon Resource Name (ARN) of the Network Load Balancer that is
-- associated with the target.
createTrafficMirrorTarget_networkLoadBalancerArn :: Lens.Lens' CreateTrafficMirrorTarget (Prelude.Maybe Prelude.Text)
createTrafficMirrorTarget_networkLoadBalancerArn = Lens.lens (\CreateTrafficMirrorTarget' {networkLoadBalancerArn} -> networkLoadBalancerArn) (\s@CreateTrafficMirrorTarget' {} a -> s {networkLoadBalancerArn = a} :: CreateTrafficMirrorTarget)

-- | The tags to assign to the Traffic Mirror target.
createTrafficMirrorTarget_tagSpecifications :: Lens.Lens' CreateTrafficMirrorTarget (Prelude.Maybe [TagSpecification])
createTrafficMirrorTarget_tagSpecifications = Lens.lens (\CreateTrafficMirrorTarget' {tagSpecifications} -> tagSpecifications) (\s@CreateTrafficMirrorTarget' {} a -> s {tagSpecifications = a} :: CreateTrafficMirrorTarget) Prelude.. Lens.mapping Lens.coerced

-- | The description of the Traffic Mirror target.
createTrafficMirrorTarget_description :: Lens.Lens' CreateTrafficMirrorTarget (Prelude.Maybe Prelude.Text)
createTrafficMirrorTarget_description = Lens.lens (\CreateTrafficMirrorTarget' {description} -> description) (\s@CreateTrafficMirrorTarget' {} a -> s {description = a} :: CreateTrafficMirrorTarget)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
createTrafficMirrorTarget_dryRun :: Lens.Lens' CreateTrafficMirrorTarget (Prelude.Maybe Prelude.Bool)
createTrafficMirrorTarget_dryRun = Lens.lens (\CreateTrafficMirrorTarget' {dryRun} -> dryRun) (\s@CreateTrafficMirrorTarget' {} a -> s {dryRun = a} :: CreateTrafficMirrorTarget)

instance Core.AWSRequest CreateTrafficMirrorTarget where
  type
    AWSResponse CreateTrafficMirrorTarget =
      CreateTrafficMirrorTargetResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          CreateTrafficMirrorTargetResponse'
            Prelude.<$> (x Core..@? "clientToken")
            Prelude.<*> (x Core..@? "trafficMirrorTarget")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateTrafficMirrorTarget where
  hashWithSalt salt' CreateTrafficMirrorTarget' {..} =
    salt' `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` tagSpecifications
      `Prelude.hashWithSalt` networkLoadBalancerArn
      `Prelude.hashWithSalt` networkInterfaceId
      `Prelude.hashWithSalt` clientToken

instance Prelude.NFData CreateTrafficMirrorTarget where
  rnf CreateTrafficMirrorTarget' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf tagSpecifications
      `Prelude.seq` Prelude.rnf networkLoadBalancerArn
      `Prelude.seq` Prelude.rnf networkInterfaceId

instance Core.ToHeaders CreateTrafficMirrorTarget where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath CreateTrafficMirrorTarget where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateTrafficMirrorTarget where
  toQuery CreateTrafficMirrorTarget' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("CreateTrafficMirrorTarget" :: Prelude.ByteString),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        "ClientToken" Core.=: clientToken,
        "NetworkInterfaceId" Core.=: networkInterfaceId,
        "NetworkLoadBalancerArn"
          Core.=: networkLoadBalancerArn,
        Core.toQuery
          ( Core.toQueryList "TagSpecification"
              Prelude.<$> tagSpecifications
          ),
        "Description" Core.=: description,
        "DryRun" Core.=: dryRun
      ]

-- | /See:/ 'newCreateTrafficMirrorTargetResponse' smart constructor.
data CreateTrafficMirrorTargetResponse = CreateTrafficMirrorTargetResponse'
  { -- | Unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to ensure idempotency>.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | Information about the Traffic Mirror target.
    trafficMirrorTarget :: Prelude.Maybe TrafficMirrorTarget,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateTrafficMirrorTargetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'createTrafficMirrorTargetResponse_clientToken' - Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to ensure idempotency>.
--
-- 'trafficMirrorTarget', 'createTrafficMirrorTargetResponse_trafficMirrorTarget' - Information about the Traffic Mirror target.
--
-- 'httpStatus', 'createTrafficMirrorTargetResponse_httpStatus' - The response's http status code.
newCreateTrafficMirrorTargetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateTrafficMirrorTargetResponse
newCreateTrafficMirrorTargetResponse pHttpStatus_ =
  CreateTrafficMirrorTargetResponse'
    { clientToken =
        Prelude.Nothing,
      trafficMirrorTarget = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to ensure idempotency>.
createTrafficMirrorTargetResponse_clientToken :: Lens.Lens' CreateTrafficMirrorTargetResponse (Prelude.Maybe Prelude.Text)
createTrafficMirrorTargetResponse_clientToken = Lens.lens (\CreateTrafficMirrorTargetResponse' {clientToken} -> clientToken) (\s@CreateTrafficMirrorTargetResponse' {} a -> s {clientToken = a} :: CreateTrafficMirrorTargetResponse)

-- | Information about the Traffic Mirror target.
createTrafficMirrorTargetResponse_trafficMirrorTarget :: Lens.Lens' CreateTrafficMirrorTargetResponse (Prelude.Maybe TrafficMirrorTarget)
createTrafficMirrorTargetResponse_trafficMirrorTarget = Lens.lens (\CreateTrafficMirrorTargetResponse' {trafficMirrorTarget} -> trafficMirrorTarget) (\s@CreateTrafficMirrorTargetResponse' {} a -> s {trafficMirrorTarget = a} :: CreateTrafficMirrorTargetResponse)

-- | The response's http status code.
createTrafficMirrorTargetResponse_httpStatus :: Lens.Lens' CreateTrafficMirrorTargetResponse Prelude.Int
createTrafficMirrorTargetResponse_httpStatus = Lens.lens (\CreateTrafficMirrorTargetResponse' {httpStatus} -> httpStatus) (\s@CreateTrafficMirrorTargetResponse' {} a -> s {httpStatus = a} :: CreateTrafficMirrorTargetResponse)

instance
  Prelude.NFData
    CreateTrafficMirrorTargetResponse
  where
  rnf CreateTrafficMirrorTargetResponse' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf trafficMirrorTarget
