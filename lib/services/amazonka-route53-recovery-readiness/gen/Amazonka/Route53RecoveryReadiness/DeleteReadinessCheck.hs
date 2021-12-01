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
-- Module      : Amazonka.Route53RecoveryReadiness.DeleteReadinessCheck
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an existing Readiness Check.
module Amazonka.Route53RecoveryReadiness.DeleteReadinessCheck
  ( -- * Creating a Request
    DeleteReadinessCheck (..),
    newDeleteReadinessCheck,

    -- * Request Lenses
    deleteReadinessCheck_readinessCheckName,

    -- * Destructuring the Response
    DeleteReadinessCheckResponse (..),
    newDeleteReadinessCheckResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53RecoveryReadiness.Types

-- | /See:/ 'newDeleteReadinessCheck' smart constructor.
data DeleteReadinessCheck = DeleteReadinessCheck'
  { -- | The ReadinessCheck to delete
    readinessCheckName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteReadinessCheck' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'readinessCheckName', 'deleteReadinessCheck_readinessCheckName' - The ReadinessCheck to delete
newDeleteReadinessCheck ::
  -- | 'readinessCheckName'
  Prelude.Text ->
  DeleteReadinessCheck
newDeleteReadinessCheck pReadinessCheckName_ =
  DeleteReadinessCheck'
    { readinessCheckName =
        pReadinessCheckName_
    }

-- | The ReadinessCheck to delete
deleteReadinessCheck_readinessCheckName :: Lens.Lens' DeleteReadinessCheck Prelude.Text
deleteReadinessCheck_readinessCheckName = Lens.lens (\DeleteReadinessCheck' {readinessCheckName} -> readinessCheckName) (\s@DeleteReadinessCheck' {} a -> s {readinessCheckName = a} :: DeleteReadinessCheck)

instance Core.AWSRequest DeleteReadinessCheck where
  type
    AWSResponse DeleteReadinessCheck =
      DeleteReadinessCheckResponse
  request = Request.delete defaultService
  response =
    Response.receiveNull DeleteReadinessCheckResponse'

instance Prelude.Hashable DeleteReadinessCheck where
  hashWithSalt salt' DeleteReadinessCheck' {..} =
    salt' `Prelude.hashWithSalt` readinessCheckName

instance Prelude.NFData DeleteReadinessCheck where
  rnf DeleteReadinessCheck' {..} =
    Prelude.rnf readinessCheckName

instance Core.ToHeaders DeleteReadinessCheck where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath DeleteReadinessCheck where
  toPath DeleteReadinessCheck' {..} =
    Prelude.mconcat
      ["/readinesschecks/", Core.toBS readinessCheckName]

instance Core.ToQuery DeleteReadinessCheck where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteReadinessCheckResponse' smart constructor.
data DeleteReadinessCheckResponse = DeleteReadinessCheckResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteReadinessCheckResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteReadinessCheckResponse ::
  DeleteReadinessCheckResponse
newDeleteReadinessCheckResponse =
  DeleteReadinessCheckResponse'

instance Prelude.NFData DeleteReadinessCheckResponse where
  rnf _ = ()
