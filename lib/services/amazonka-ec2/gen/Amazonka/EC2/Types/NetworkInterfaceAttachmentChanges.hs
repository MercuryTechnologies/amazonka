{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.EC2.Types.NetworkInterfaceAttachmentChanges
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.NetworkInterfaceAttachmentChanges where

import qualified Amazonka.Core as Core
import Amazonka.EC2.Internal
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes an attachment change.
--
-- /See:/ 'newNetworkInterfaceAttachmentChanges' smart constructor.
data NetworkInterfaceAttachmentChanges = NetworkInterfaceAttachmentChanges'
  { -- | Indicates whether the network interface is deleted when the instance is
    -- terminated.
    deleteOnTermination :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the network interface attachment.
    attachmentId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NetworkInterfaceAttachmentChanges' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deleteOnTermination', 'networkInterfaceAttachmentChanges_deleteOnTermination' - Indicates whether the network interface is deleted when the instance is
-- terminated.
--
-- 'attachmentId', 'networkInterfaceAttachmentChanges_attachmentId' - The ID of the network interface attachment.
newNetworkInterfaceAttachmentChanges ::
  NetworkInterfaceAttachmentChanges
newNetworkInterfaceAttachmentChanges =
  NetworkInterfaceAttachmentChanges'
    { deleteOnTermination =
        Prelude.Nothing,
      attachmentId = Prelude.Nothing
    }

-- | Indicates whether the network interface is deleted when the instance is
-- terminated.
networkInterfaceAttachmentChanges_deleteOnTermination :: Lens.Lens' NetworkInterfaceAttachmentChanges (Prelude.Maybe Prelude.Bool)
networkInterfaceAttachmentChanges_deleteOnTermination = Lens.lens (\NetworkInterfaceAttachmentChanges' {deleteOnTermination} -> deleteOnTermination) (\s@NetworkInterfaceAttachmentChanges' {} a -> s {deleteOnTermination = a} :: NetworkInterfaceAttachmentChanges)

-- | The ID of the network interface attachment.
networkInterfaceAttachmentChanges_attachmentId :: Lens.Lens' NetworkInterfaceAttachmentChanges (Prelude.Maybe Prelude.Text)
networkInterfaceAttachmentChanges_attachmentId = Lens.lens (\NetworkInterfaceAttachmentChanges' {attachmentId} -> attachmentId) (\s@NetworkInterfaceAttachmentChanges' {} a -> s {attachmentId = a} :: NetworkInterfaceAttachmentChanges)

instance
  Prelude.Hashable
    NetworkInterfaceAttachmentChanges
  where
  hashWithSalt
    salt'
    NetworkInterfaceAttachmentChanges' {..} =
      salt' `Prelude.hashWithSalt` attachmentId
        `Prelude.hashWithSalt` deleteOnTermination

instance
  Prelude.NFData
    NetworkInterfaceAttachmentChanges
  where
  rnf NetworkInterfaceAttachmentChanges' {..} =
    Prelude.rnf deleteOnTermination
      `Prelude.seq` Prelude.rnf attachmentId

instance
  Core.ToQuery
    NetworkInterfaceAttachmentChanges
  where
  toQuery NetworkInterfaceAttachmentChanges' {..} =
    Prelude.mconcat
      [ "DeleteOnTermination" Core.=: deleteOnTermination,
        "AttachmentId" Core.=: attachmentId
      ]
