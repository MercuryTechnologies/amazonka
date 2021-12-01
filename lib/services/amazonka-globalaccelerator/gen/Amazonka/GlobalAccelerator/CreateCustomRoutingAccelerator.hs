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
-- Module      : Amazonka.GlobalAccelerator.CreateCustomRoutingAccelerator
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create a custom routing accelerator. A custom routing accelerator
-- directs traffic to one of possibly thousands of Amazon EC2 instance
-- destinations running in a single or multiple virtual private clouds
-- (VPC) subnet endpoints.
--
-- Be aware that, by default, all destination EC2 instances in a VPC subnet
-- endpoint cannot receive traffic. To enable all destinations to receive
-- traffic, or to specify individual port mappings that can receive
-- traffic, see the
-- <https://docs.aws.amazon.com/global-accelerator/latest/api/API_AllowCustomRoutingTraffic.html AllowCustomRoutingTraffic>
-- operation.
--
-- Global Accelerator is a global service that supports endpoints in
-- multiple AWS Regions but you must specify the US West (Oregon) Region to
-- create or update accelerators.
module Amazonka.GlobalAccelerator.CreateCustomRoutingAccelerator
  ( -- * Creating a Request
    CreateCustomRoutingAccelerator (..),
    newCreateCustomRoutingAccelerator,

    -- * Request Lenses
    createCustomRoutingAccelerator_enabled,
    createCustomRoutingAccelerator_ipAddressType,
    createCustomRoutingAccelerator_tags,
    createCustomRoutingAccelerator_ipAddresses,
    createCustomRoutingAccelerator_name,
    createCustomRoutingAccelerator_idempotencyToken,

    -- * Destructuring the Response
    CreateCustomRoutingAcceleratorResponse (..),
    newCreateCustomRoutingAcceleratorResponse,

    -- * Response Lenses
    createCustomRoutingAcceleratorResponse_accelerator,
    createCustomRoutingAcceleratorResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.GlobalAccelerator.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateCustomRoutingAccelerator' smart constructor.
data CreateCustomRoutingAccelerator = CreateCustomRoutingAccelerator'
  { -- | Indicates whether an accelerator is enabled. The value is true or false.
    -- The default value is true.
    --
    -- If the value is set to true, an accelerator cannot be deleted. If set to
    -- false, the accelerator can be deleted.
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | The value for the address type must be IPv4.
    ipAddressType :: Prelude.Maybe IpAddressType,
    -- | Create tags for an accelerator.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/global-accelerator/latest/dg/tagging-in-global-accelerator.html Tagging in AWS Global Accelerator>
    -- in the /AWS Global Accelerator Developer Guide/.
    tags :: Prelude.Maybe [Tag],
    -- | Optionally, if you\'ve added your own IP address pool to Global
    -- Accelerator (BYOIP), you can choose IP addresses from your own pool to
    -- use for the accelerator\'s static IP addresses when you create an
    -- accelerator. You can specify one or two addresses, separated by a space.
    -- Do not include the \/32 suffix.
    --
    -- Only one IP address from each of your IP address ranges can be used for
    -- each accelerator. If you specify only one IP address from your IP
    -- address range, Global Accelerator assigns a second static IP address for
    -- the accelerator from the AWS IP address pool.
    --
    -- Note that you can\'t update IP addresses for an existing accelerator. To
    -- change them, you must create a new accelerator with the new addresses.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/global-accelerator/latest/dg/using-byoip.html Bring your own IP addresses (BYOIP)>
    -- in the /AWS Global Accelerator Developer Guide/.
    ipAddresses :: Prelude.Maybe [Prelude.Text],
    -- | The name of a custom routing accelerator. The name can have a maximum of
    -- 64 characters, must contain only alphanumeric characters or hyphens (-),
    -- and must not begin or end with a hyphen.
    name :: Prelude.Text,
    -- | A unique, case-sensitive identifier that you provide to ensure the
    -- idempotency—that is, the uniqueness—of the request.
    idempotencyToken :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateCustomRoutingAccelerator' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enabled', 'createCustomRoutingAccelerator_enabled' - Indicates whether an accelerator is enabled. The value is true or false.
-- The default value is true.
--
-- If the value is set to true, an accelerator cannot be deleted. If set to
-- false, the accelerator can be deleted.
--
-- 'ipAddressType', 'createCustomRoutingAccelerator_ipAddressType' - The value for the address type must be IPv4.
--
-- 'tags', 'createCustomRoutingAccelerator_tags' - Create tags for an accelerator.
--
-- For more information, see
-- <https://docs.aws.amazon.com/global-accelerator/latest/dg/tagging-in-global-accelerator.html Tagging in AWS Global Accelerator>
-- in the /AWS Global Accelerator Developer Guide/.
--
-- 'ipAddresses', 'createCustomRoutingAccelerator_ipAddresses' - Optionally, if you\'ve added your own IP address pool to Global
-- Accelerator (BYOIP), you can choose IP addresses from your own pool to
-- use for the accelerator\'s static IP addresses when you create an
-- accelerator. You can specify one or two addresses, separated by a space.
-- Do not include the \/32 suffix.
--
-- Only one IP address from each of your IP address ranges can be used for
-- each accelerator. If you specify only one IP address from your IP
-- address range, Global Accelerator assigns a second static IP address for
-- the accelerator from the AWS IP address pool.
--
-- Note that you can\'t update IP addresses for an existing accelerator. To
-- change them, you must create a new accelerator with the new addresses.
--
-- For more information, see
-- <https://docs.aws.amazon.com/global-accelerator/latest/dg/using-byoip.html Bring your own IP addresses (BYOIP)>
-- in the /AWS Global Accelerator Developer Guide/.
--
-- 'name', 'createCustomRoutingAccelerator_name' - The name of a custom routing accelerator. The name can have a maximum of
-- 64 characters, must contain only alphanumeric characters or hyphens (-),
-- and must not begin or end with a hyphen.
--
-- 'idempotencyToken', 'createCustomRoutingAccelerator_idempotencyToken' - A unique, case-sensitive identifier that you provide to ensure the
-- idempotency—that is, the uniqueness—of the request.
newCreateCustomRoutingAccelerator ::
  -- | 'name'
  Prelude.Text ->
  -- | 'idempotencyToken'
  Prelude.Text ->
  CreateCustomRoutingAccelerator
newCreateCustomRoutingAccelerator
  pName_
  pIdempotencyToken_ =
    CreateCustomRoutingAccelerator'
      { enabled =
          Prelude.Nothing,
        ipAddressType = Prelude.Nothing,
        tags = Prelude.Nothing,
        ipAddresses = Prelude.Nothing,
        name = pName_,
        idempotencyToken = pIdempotencyToken_
      }

-- | Indicates whether an accelerator is enabled. The value is true or false.
-- The default value is true.
--
-- If the value is set to true, an accelerator cannot be deleted. If set to
-- false, the accelerator can be deleted.
createCustomRoutingAccelerator_enabled :: Lens.Lens' CreateCustomRoutingAccelerator (Prelude.Maybe Prelude.Bool)
createCustomRoutingAccelerator_enabled = Lens.lens (\CreateCustomRoutingAccelerator' {enabled} -> enabled) (\s@CreateCustomRoutingAccelerator' {} a -> s {enabled = a} :: CreateCustomRoutingAccelerator)

-- | The value for the address type must be IPv4.
createCustomRoutingAccelerator_ipAddressType :: Lens.Lens' CreateCustomRoutingAccelerator (Prelude.Maybe IpAddressType)
createCustomRoutingAccelerator_ipAddressType = Lens.lens (\CreateCustomRoutingAccelerator' {ipAddressType} -> ipAddressType) (\s@CreateCustomRoutingAccelerator' {} a -> s {ipAddressType = a} :: CreateCustomRoutingAccelerator)

-- | Create tags for an accelerator.
--
-- For more information, see
-- <https://docs.aws.amazon.com/global-accelerator/latest/dg/tagging-in-global-accelerator.html Tagging in AWS Global Accelerator>
-- in the /AWS Global Accelerator Developer Guide/.
createCustomRoutingAccelerator_tags :: Lens.Lens' CreateCustomRoutingAccelerator (Prelude.Maybe [Tag])
createCustomRoutingAccelerator_tags = Lens.lens (\CreateCustomRoutingAccelerator' {tags} -> tags) (\s@CreateCustomRoutingAccelerator' {} a -> s {tags = a} :: CreateCustomRoutingAccelerator) Prelude.. Lens.mapping Lens.coerced

-- | Optionally, if you\'ve added your own IP address pool to Global
-- Accelerator (BYOIP), you can choose IP addresses from your own pool to
-- use for the accelerator\'s static IP addresses when you create an
-- accelerator. You can specify one or two addresses, separated by a space.
-- Do not include the \/32 suffix.
--
-- Only one IP address from each of your IP address ranges can be used for
-- each accelerator. If you specify only one IP address from your IP
-- address range, Global Accelerator assigns a second static IP address for
-- the accelerator from the AWS IP address pool.
--
-- Note that you can\'t update IP addresses for an existing accelerator. To
-- change them, you must create a new accelerator with the new addresses.
--
-- For more information, see
-- <https://docs.aws.amazon.com/global-accelerator/latest/dg/using-byoip.html Bring your own IP addresses (BYOIP)>
-- in the /AWS Global Accelerator Developer Guide/.
createCustomRoutingAccelerator_ipAddresses :: Lens.Lens' CreateCustomRoutingAccelerator (Prelude.Maybe [Prelude.Text])
createCustomRoutingAccelerator_ipAddresses = Lens.lens (\CreateCustomRoutingAccelerator' {ipAddresses} -> ipAddresses) (\s@CreateCustomRoutingAccelerator' {} a -> s {ipAddresses = a} :: CreateCustomRoutingAccelerator) Prelude.. Lens.mapping Lens.coerced

-- | The name of a custom routing accelerator. The name can have a maximum of
-- 64 characters, must contain only alphanumeric characters or hyphens (-),
-- and must not begin or end with a hyphen.
createCustomRoutingAccelerator_name :: Lens.Lens' CreateCustomRoutingAccelerator Prelude.Text
createCustomRoutingAccelerator_name = Lens.lens (\CreateCustomRoutingAccelerator' {name} -> name) (\s@CreateCustomRoutingAccelerator' {} a -> s {name = a} :: CreateCustomRoutingAccelerator)

-- | A unique, case-sensitive identifier that you provide to ensure the
-- idempotency—that is, the uniqueness—of the request.
createCustomRoutingAccelerator_idempotencyToken :: Lens.Lens' CreateCustomRoutingAccelerator Prelude.Text
createCustomRoutingAccelerator_idempotencyToken = Lens.lens (\CreateCustomRoutingAccelerator' {idempotencyToken} -> idempotencyToken) (\s@CreateCustomRoutingAccelerator' {} a -> s {idempotencyToken = a} :: CreateCustomRoutingAccelerator)

instance
  Core.AWSRequest
    CreateCustomRoutingAccelerator
  where
  type
    AWSResponse CreateCustomRoutingAccelerator =
      CreateCustomRoutingAcceleratorResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateCustomRoutingAcceleratorResponse'
            Prelude.<$> (x Core..?> "Accelerator")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    CreateCustomRoutingAccelerator
  where
  hashWithSalt
    salt'
    CreateCustomRoutingAccelerator' {..} =
      salt' `Prelude.hashWithSalt` idempotencyToken
        `Prelude.hashWithSalt` name
        `Prelude.hashWithSalt` ipAddresses
        `Prelude.hashWithSalt` tags
        `Prelude.hashWithSalt` ipAddressType
        `Prelude.hashWithSalt` enabled

instance
  Prelude.NFData
    CreateCustomRoutingAccelerator
  where
  rnf CreateCustomRoutingAccelerator' {..} =
    Prelude.rnf enabled
      `Prelude.seq` Prelude.rnf idempotencyToken
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf ipAddresses
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf ipAddressType

instance
  Core.ToHeaders
    CreateCustomRoutingAccelerator
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "GlobalAccelerator_V20180706.CreateCustomRoutingAccelerator" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateCustomRoutingAccelerator where
  toJSON CreateCustomRoutingAccelerator' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Enabled" Core..=) Prelude.<$> enabled,
            ("IpAddressType" Core..=) Prelude.<$> ipAddressType,
            ("Tags" Core..=) Prelude.<$> tags,
            ("IpAddresses" Core..=) Prelude.<$> ipAddresses,
            Prelude.Just ("Name" Core..= name),
            Prelude.Just
              ("IdempotencyToken" Core..= idempotencyToken)
          ]
      )

instance Core.ToPath CreateCustomRoutingAccelerator where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateCustomRoutingAccelerator where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateCustomRoutingAcceleratorResponse' smart constructor.
data CreateCustomRoutingAcceleratorResponse = CreateCustomRoutingAcceleratorResponse'
  { -- | The accelerator that is created.
    accelerator :: Prelude.Maybe CustomRoutingAccelerator,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateCustomRoutingAcceleratorResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accelerator', 'createCustomRoutingAcceleratorResponse_accelerator' - The accelerator that is created.
--
-- 'httpStatus', 'createCustomRoutingAcceleratorResponse_httpStatus' - The response's http status code.
newCreateCustomRoutingAcceleratorResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateCustomRoutingAcceleratorResponse
newCreateCustomRoutingAcceleratorResponse
  pHttpStatus_ =
    CreateCustomRoutingAcceleratorResponse'
      { accelerator =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The accelerator that is created.
createCustomRoutingAcceleratorResponse_accelerator :: Lens.Lens' CreateCustomRoutingAcceleratorResponse (Prelude.Maybe CustomRoutingAccelerator)
createCustomRoutingAcceleratorResponse_accelerator = Lens.lens (\CreateCustomRoutingAcceleratorResponse' {accelerator} -> accelerator) (\s@CreateCustomRoutingAcceleratorResponse' {} a -> s {accelerator = a} :: CreateCustomRoutingAcceleratorResponse)

-- | The response's http status code.
createCustomRoutingAcceleratorResponse_httpStatus :: Lens.Lens' CreateCustomRoutingAcceleratorResponse Prelude.Int
createCustomRoutingAcceleratorResponse_httpStatus = Lens.lens (\CreateCustomRoutingAcceleratorResponse' {httpStatus} -> httpStatus) (\s@CreateCustomRoutingAcceleratorResponse' {} a -> s {httpStatus = a} :: CreateCustomRoutingAcceleratorResponse)

instance
  Prelude.NFData
    CreateCustomRoutingAcceleratorResponse
  where
  rnf CreateCustomRoutingAcceleratorResponse' {..} =
    Prelude.rnf accelerator
      `Prelude.seq` Prelude.rnf httpStatus
