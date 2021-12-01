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
-- Module      : Amazonka.EC2.DeleteKeyPair
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified key pair, by removing the public key from Amazon
-- EC2.
module Amazonka.EC2.DeleteKeyPair
  ( -- * Creating a Request
    DeleteKeyPair (..),
    newDeleteKeyPair,

    -- * Request Lenses
    deleteKeyPair_keyName,
    deleteKeyPair_keyPairId,
    deleteKeyPair_dryRun,

    -- * Destructuring the Response
    DeleteKeyPairResponse (..),
    newDeleteKeyPairResponse,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.EC2.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteKeyPair' smart constructor.
data DeleteKeyPair = DeleteKeyPair'
  { -- | The name of the key pair.
    keyName :: Prelude.Maybe Prelude.Text,
    -- | The ID of the key pair.
    keyPairId :: Prelude.Maybe Prelude.Text,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteKeyPair' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'keyName', 'deleteKeyPair_keyName' - The name of the key pair.
--
-- 'keyPairId', 'deleteKeyPair_keyPairId' - The ID of the key pair.
--
-- 'dryRun', 'deleteKeyPair_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
newDeleteKeyPair ::
  DeleteKeyPair
newDeleteKeyPair =
  DeleteKeyPair'
    { keyName = Prelude.Nothing,
      keyPairId = Prelude.Nothing,
      dryRun = Prelude.Nothing
    }

-- | The name of the key pair.
deleteKeyPair_keyName :: Lens.Lens' DeleteKeyPair (Prelude.Maybe Prelude.Text)
deleteKeyPair_keyName = Lens.lens (\DeleteKeyPair' {keyName} -> keyName) (\s@DeleteKeyPair' {} a -> s {keyName = a} :: DeleteKeyPair)

-- | The ID of the key pair.
deleteKeyPair_keyPairId :: Lens.Lens' DeleteKeyPair (Prelude.Maybe Prelude.Text)
deleteKeyPair_keyPairId = Lens.lens (\DeleteKeyPair' {keyPairId} -> keyPairId) (\s@DeleteKeyPair' {} a -> s {keyPairId = a} :: DeleteKeyPair)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
deleteKeyPair_dryRun :: Lens.Lens' DeleteKeyPair (Prelude.Maybe Prelude.Bool)
deleteKeyPair_dryRun = Lens.lens (\DeleteKeyPair' {dryRun} -> dryRun) (\s@DeleteKeyPair' {} a -> s {dryRun = a} :: DeleteKeyPair)

instance Core.AWSRequest DeleteKeyPair where
  type
    AWSResponse DeleteKeyPair =
      DeleteKeyPairResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull DeleteKeyPairResponse'

instance Prelude.Hashable DeleteKeyPair where
  hashWithSalt salt' DeleteKeyPair' {..} =
    salt' `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` keyPairId
      `Prelude.hashWithSalt` keyName

instance Prelude.NFData DeleteKeyPair where
  rnf DeleteKeyPair' {..} =
    Prelude.rnf keyName
      `Prelude.seq` Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf keyPairId

instance Core.ToHeaders DeleteKeyPair where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DeleteKeyPair where
  toPath = Prelude.const "/"

instance Core.ToQuery DeleteKeyPair where
  toQuery DeleteKeyPair' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("DeleteKeyPair" :: Prelude.ByteString),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        "KeyName" Core.=: keyName,
        "KeyPairId" Core.=: keyPairId,
        "DryRun" Core.=: dryRun
      ]

-- | /See:/ 'newDeleteKeyPairResponse' smart constructor.
data DeleteKeyPairResponse = DeleteKeyPairResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteKeyPairResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteKeyPairResponse ::
  DeleteKeyPairResponse
newDeleteKeyPairResponse = DeleteKeyPairResponse'

instance Prelude.NFData DeleteKeyPairResponse where
  rnf _ = ()
